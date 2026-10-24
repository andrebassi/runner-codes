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

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// ReadyTimeout é quanto o boot pode levar até o guest responder. O boot medido é
// ~260 ms (p95 266 ms); 30 s é o teto de "sem progresso" e cobre host
// lento e cache de página frio (primeiro boot medido em 869 ms) com folga larga.
const ReadyTimeout = 30 * time.Second

// maxBootsPerLanguage limita boots simultâneos por linguagem na reposição, para um pool
// grande não disparar dezenas de VMs de uma vez.
const maxBootsPerLanguage = 2

// DefaultMaxConcurrentBoots é o teto de boots simultâneos da reposição somando todas as
// linguagens. Sem ele o serve subia 15 × 2 = 30 VMs juntas, saturava a CPU e o job do
// pedido levava 1,7–2,3 s em vez de ~40 ms (medido). 4 deixa a reposição
// andar sem disputar a CPU com o job do pedido.
const DefaultMaxConcurrentBoots = 4

// DefaultMaxGPUVMs é o teto de VMs com GPU vivas ao mesmo tempo. Todas dividem a mesma GPU
// do host, e cada uma reserva a janela de VRAM; duas cobrem um job e uma comparação sem que
// um laço de pedidos esgote a GPU de quem está usando o Mac.
const DefaultMaxGPUVMs = 2

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
	// GPU marca VM com virtio-gpu: ela ocupa uma vaga de GPU até o Destroy.
	GPU bool
}

// Pool mantém VMs prontas por linguagem. Cada VM é entregue uma única vez.
type Pool struct {
	Launcher ports.VMLauncher
	Guest    ports.GuestClient
	Images   ports.ImageStore
	Catalog  Catalog
	WorkDir  string
	Size     int
	// MaxConcurrentBoots limita a reposição no total; 0 usa DefaultMaxConcurrentBoots.
	// O boot frio do caminho do pedido não passa por aqui: esperar fila somaria latência.
	MaxConcurrentBoots int
	// MaxGPUVMs limita VMs com GPU vivas; 0 usa DefaultMaxGPUVMs.
	MaxGPUVMs int
	Log       io.Writer

	bootSlots chan struct{}
	gpuSlots  chan struct{}

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
	limit := p.MaxConcurrentBoots
	if limit <= 0 {
		limit = DefaultMaxConcurrentBoots
	}
	p.bootSlots = make(chan struct{}, limit)
	gpu := p.MaxGPUVMs
	if gpu <= 0 {
		gpu = DefaultMaxGPUVMs
	}
	p.gpuSlots = make(chan struct{}, gpu)
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
		select {
		case <-p.ctx.Done():
			tokens <- struct{}{}
			return
		case p.bootSlots <- struct{}{}:
		}
		vm, err := p.Boot(p.ctx, lang)
		<-p.bootSlots
		if err != nil {
			tokens <- struct{}{}
			if p.ctx.Err() != nil {
				return
			}
			p.logf("pool %s: boot failed: %v", lang, err)
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
	return p.boot(ctx, lang, false)
}

// TakeGPU sobe, a frio, uma VM com GPU. Não passa pelo pool: VM com GPU segura a janela de
// VRAM e uma vaga da GPU do host, então não fica parada esperando pedido. Espera vaga
// (DefaultMaxGPUVMs) até o ctx vencer; a vaga volta no Destroy.
func (p *Pool) TakeGPU(ctx context.Context, lang string) (*ReadyVM, error) {
	select {
	case p.gpuSlots <- struct{}{}:
	case <-ctx.Done():
		return nil, fmt.Errorf("wait for a free GPU slot: %w", ctx.Err())
	}
	r, err := p.boot(ctx, lang, true)
	if err != nil {
		<-p.gpuSlots
		return nil, err
	}
	return r, nil
}

// boot é o boot comum; gpu liga o virtio-gpu com a VRAM do catálogo.
func (p *Pool) boot(ctx context.Context, lang string, gpu bool) (*ReadyVM, error) {
	l, ok := p.Catalog.Get(lang)
	if !ok {
		return nil, domain.ErrUnsupportedLanguage{Name: lang}
	}
	image, ok := p.Images.ImagePath(lang)
	if !ok {
		return nil, fmt.Errorf("image for %s not built (run `rc image build --stack %s`)", lang, lang)
	}
	if err := os.MkdirAll(p.WorkDir, 0o700); err != nil {
		return nil, err
	}
	// Diretório exclusivo da VM: o Landlock do processo vmm libera escrita só nele, então
	// uma VM comprometida não toca no socket nem no console de outra.
	vmDir := filepath.Join(p.WorkDir, shortID())
	if err := os.Mkdir(vmDir, 0o700); err != nil {
		return nil, err
	}
	r := &ReadyVM{Language: lang, Socket: filepath.Join(vmDir, "v.sock"), Console: filepath.Join(vmDir, "console")}
	spec := ports.VMSpec{
		CPUs: l.CPUs, MemoryMiB: l.MemoryMiB, RootDisk: image,
		VsockSocket: r.Socket, VsockPort: protocol.DefaultPort, Console: r.Console,
		Exec: "/opt/runner/start.sh",
	}
	if gpu {
		spec.GPU, spec.GPUVRAMMiB = true, l.GPUVRAMMiB
	}
	vm, err := p.Launcher.StartVM(spec)
	if err != nil {
		os.RemoveAll(vmDir)
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
		// Destroy sem a marca de GPU: quem segura a vaga é o TakeGPU, que a devolve no erro.
		p.Destroy(r)
		return nil, fmt.Errorf("VM for %s did not become ready: %w; console: %s", lang, err, console)
	}
	r.GPU = gpu
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
	os.RemoveAll(filepath.Dir(r.Socket))
	if r.GPU {
		r.GPU = false // Destroy idempotente: a vaga volta uma vez só
		<-p.gpuSlots
	}
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

// Status devolve quantas VMs estão prontas por linguagem (para o /health).
func (p *Pool) Status() map[string]int {
	p.mu.Lock()
	defer p.mu.Unlock()
	out := make(map[string]int, len(p.ready))
	for lang, ch := range p.ready {
		out[lang] = len(ch)
	}
	return out
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
		return "(no console)"
	}
	defer f.Close()
	st, _ := f.Stat()
	if st.Size() > n {
		f.Seek(st.Size()-n, io.SeekStart)
	}
	b, _ := io.ReadAll(f)
	return string(b)
}
