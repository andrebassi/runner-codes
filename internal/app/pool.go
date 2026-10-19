// Package app é o núcleo do runner: o pool de VMs pré-bootadas e o serviço que executa
// um job numa VM nova. Só conhece as portas (internal/ports) e o domínio.
package app

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// ReadyTimeout é quanto o boot pode levar até o guest responder. O boot medido é
// ~260 ms (p95 266 ms); 30 s é o mesmo teto de "sem progresso" do smolvm e cobre host
// lento e cache de página frio (primeiro boot medido em 869 ms) com folga larga.
const ReadyTimeout = 30 * time.Second

// maxBootsPerLanguage limita boots simultâneos por linguagem na reposição, para um pool
// grande não disparar dezenas de VMs de uma vez (o smolvm limita a 4, pool_controller.rs:19).
const maxBootsPerLanguage = 2

// retryBackoff é a espera depois de um boot que falhou, antes de tentar repor de novo;
// sem ela um defeito de imagem viraria um laço de boot consumindo CPU.
var retryBackoff = 2 * time.Second

// Catalog é o que o pool precisa saber de uma linguagem.
type Catalog interface {
	Get(name string) (domain.Language, bool)
}

// ReadyVM é uma VM bootada com o guest respondendo, ainda não usada.
type ReadyVM struct {
	VM       ports.VM
	Language string
	Socket   string
	Console  string
}

// Pool mantém VMs prontas por linguagem. Cada VM é entregue uma única vez.
type Pool struct {
	Launcher ports.VMLauncher
	Guest    ports.GuestClient
	Images   ports.ImageStore
	Catalog  Catalog
	WorkDir  string
	Size     int
	Log      io.Writer

	mu     sync.Mutex
	ready  map[string]chan *ReadyVM
	tokens map[string]chan struct{}
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
}

// logf escreve no log quando houver destino.
func (p *Pool) logf(format string, args ...interface{}) {
	if p.Log != nil {
		fmt.Fprintf(p.Log, format+"\n", args...)
	}
}

// Start liga a reposição das linguagens pedidas. Size 0 desliga o pool: todo job faz
// boot frio (útil para medir e para CLI de uma execução só).
func (p *Pool) Start(languages []string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.ctx, p.cancel = context.WithCancel(context.Background())
	p.ready = map[string]chan *ReadyVM{}
	p.tokens = map[string]chan struct{}{}
	if p.Size <= 0 {
		return
	}
	for _, lang := range languages {
		ready := make(chan *ReadyVM, p.Size)
		// Uma ficha por VM do pool: prontas + bootando nunca passam de Size.
		tokens := make(chan struct{}, p.Size)
		for i := 0; i < p.Size; i++ {
			tokens <- struct{}{}
		}
		p.ready[lang], p.tokens[lang] = ready, tokens
		workers := maxBootsPerLanguage
		if p.Size < workers {
			workers = p.Size
		}
		for i := 0; i < workers; i++ {
			p.wg.Add(1)
			go p.refill(lang, ready, tokens)
		}
	}
}

// refill pega uma ficha, faz boot e põe a VM na fila; repete até o pool fechar.
func (p *Pool) refill(lang string, ready chan *ReadyVM, tokens chan struct{}) {
	defer p.wg.Done()
	for {
		select {
		case <-p.ctx.Done():
			return
		case <-tokens:
		}
		vm, err := p.Boot(p.ctx, lang)
		if err != nil {
			tokens <- struct{}{}
			if p.ctx.Err() != nil {
				return
			}
			p.logf("pool %s: boot falhou: %v", lang, err)
			select {
			case <-p.ctx.Done():
				return
			case <-time.After(retryBackoff):
			}
			continue
		}
		ready <- vm // nunca bloqueia: a ficha garante vaga no buffer
	}
}

// Boot sobe uma VM da linguagem e espera o guest responder. Se a VM morrer antes de
// ficar pronta, o erro traz o fim do console (onde aparece o motivo do guest).
func (p *Pool) Boot(ctx context.Context, lang string) (*ReadyVM, error) {
	l, ok := p.Catalog.Get(lang)
	if !ok {
		return nil, domain.ErrUnsupportedLanguage{Name: lang}
	}
	image, ok := p.Images.ImagePath(lang)
	if !ok {
		return nil, fmt.Errorf("imagem de %s não construída (rode `runner.code image build --lang %s`)", lang, lang)
	}
	if err := os.MkdirAll(p.WorkDir, 0o700); err != nil {
		return nil, err
	}
	id := shortID()
	r := &ReadyVM{Language: lang, Socket: filepath.Join(p.WorkDir, id+".sock"), Console: filepath.Join(p.WorkDir, id+".console")}
	vm, err := p.Launcher.StartVM(ports.VMSpec{
		CPUs: l.CPUs, MemoryMiB: l.MemoryMiB, RootDisk: image,
		VsockSocket: r.Socket, VsockPort: protocol.DefaultPort, Console: r.Console,
		Exec: "/opt/runner/start.sh",
	})
	if err != nil {
		return nil, err
	}
	r.VM = vm
	readyCtx, cancel := context.WithTimeout(ctx, ReadyTimeout)
	defer cancel()
	// A VM pode morrer no boot (imagem quebrada, hypervisor recusou): sem isto a espera
	// iria até o ReadyTimeout batendo num socket que nunca vai responder.
	go func() {
		select {
		case <-vm.Done():
			cancel()
		case <-readyCtx.Done():
		}
	}()
	if err := p.Guest.WaitReady(readyCtx, r.Socket); err != nil {
		// Lê o console antes do Destroy, que apaga o arquivo: é ali que está o motivo.
		console := tailFile(r.Console, 400)
		p.Destroy(r)
		return nil, fmt.Errorf("VM de %s não ficou pronta: %w; console: %s", lang, err, console)
	}
	return r, nil
}

// Take entrega uma VM pronta da linguagem; com o pool vazio, faz boot na hora.
// VM que morreu enquanto esperava na fila é descartada.
func (p *Pool) Take(ctx context.Context, lang string) (*ReadyVM, error) {
	p.mu.Lock()
	ready, tokens := p.ready[lang], p.tokens[lang]
	p.mu.Unlock()
	for ready != nil {
		select {
		case r := <-ready:
			tokens <- struct{}{} // libera a vaga: a reposição já começa
			select {
			case <-r.VM.Done():
				p.Destroy(r)
				continue
			default:
				return r, nil
			}
		default:
			ready = nil
		}
	}
	return p.Boot(ctx, lang)
}

// Destroy mata a VM e apaga os arquivos dela. Toda VM entregue passa por aqui.
func (p *Pool) Destroy(r *ReadyVM) {
	r.VM.Kill()
	os.Remove(r.Socket)
	os.Remove(r.Console)
}

// Close para a reposição e mata todas as VMs paradas no pool.
func (p *Pool) Close() {
	p.mu.Lock()
	cancel := p.cancel
	p.mu.Unlock()
	if cancel == nil {
		return
	}
	cancel()
	p.wg.Wait()
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, ready := range p.ready {
	drain:
		for {
			select {
			case r := <-ready:
				p.Destroy(r)
			default:
				break drain
			}
		}
	}
}

// Ready conta as VMs prontas de uma linguagem (para /health e testes).
func (p *Pool) Ready(lang string) int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return len(p.ready[lang])
}

// shortID gera um id curto para nomear socket e console.
func shortID() string {
	b := make([]byte, 6)
	rand.Read(b)
	return hex.EncodeToString(b)
}

// tailFile devolve até n bytes do fim do arquivo, para mensagens de erro.
func tailFile(path string, n int64) string {
	f, err := os.Open(path)
	if err != nil {
		return "(sem console)"
	}
	defer f.Close()
	st, _ := f.Stat()
	if st.Size() > n {
		f.Seek(st.Size()-n, io.SeekStart)
	}
	b, _ := io.ReadAll(f)
	return string(b)
}
