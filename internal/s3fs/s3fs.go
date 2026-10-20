// Package s3fs é o núcleo do volume s3:// do guest: traduz operações de arquivo em chamadas
// ao bucket. Fica separado da cola FUSE (fuse_linux.go) para que toda decisão — caminho
// para chave, o que é diretório, quando a escrita sobe, como o rename acontece — seja testada
// no host contra um S3 falso, sem /dev/fuse.
//
// Semântica (limitações declaradas): sem link simbólico, dono,
// permissão e data não persistem; escrita fica num arquivo local até o flush/fsync, e só
// então sobe inteira; rename de arquivo é CopyObject + Delete (não atômico); rename de
// diretório não existe.
package s3fs

import (
	"context"
	"errors"
	"io"
	"os"
	"path"
	"strings"
	"sync"
	"syscall"
	"time"

	"runner.code/internal/objectstore"
)

// Remote é o que o FS usa do bucket (o *objectstore.Store, ou um falso nos testes).
type Remote interface {
	List(ctx context.Context, prefix string) ([]objectstore.Entry, error)
	Head(ctx context.Context, key string) (objectstore.Entry, error)
	Get(ctx context.Context, key string, off, n int64) (io.ReadCloser, error)
	Put(ctx context.Context, key string, body io.ReadSeeker) error
	Delete(ctx context.Context, key string) error
	Copy(ctx context.Context, src, dst string) error
}

// Info é o que o FS sabe de um caminho.
type Info struct {
	Name    string
	Size    int64
	ModTime time.Time
	Dir     bool
}

// stage é a cópia local de um arquivo aberto para escrita: as escritas vão nele, e o flush
// sobe o arquivo inteiro. refs conta os handles abertos sobre o mesmo caminho (eles
// compartilham a cópia).
type stage struct {
	file  *os.File
	dirty bool
	refs  int
}

// FS é um bucket (a partir de Prefix) visto como árvore de arquivos.
type FS struct {
	remote   Remote
	prefix   string
	readOnly bool
	stageDir string
	mu       sync.Mutex
	stages   map[string]*stage
	// dirs são os diretórios criados por mkdir nesta montagem e ainda sem arquivo no bucket.
	// O S3 não tem diretório: o marcador "x/" funciona na AWS e no R2, mas um backend
	// compatível pode gravá-lo como arquivo (medido no rclone serve s3: virou um arquivo "sub"
	// e o diretório quebrou). Com o primeiro arquivo dentro, a chave dele faz o diretório existir.
	dirs map[string]bool
}

// New monta o FS sobre remote. prefix é o "diretório" do bucket montado ("" ou terminado em
// /); stageDir guarda as cópias de escrita (o tmpfs do guest).
func New(remote Remote, prefix string, readOnly bool, stageDir string) *FS {
	if prefix != "" && !strings.HasSuffix(prefix, "/") {
		prefix += "/"
	}
	return &FS{remote: remote, prefix: prefix, readOnly: readOnly, stageDir: stageDir, stages: map[string]*stage{}, dirs: map[string]bool{}}
}

// key é a chave do objeto para o caminho relativo p ("" é a raiz).
func (f *FS) key(p string) string {
	return f.prefix + strings.Trim(p, "/")
}

// remoteErr traduz o erro do bucket para errno: ausente é ENOENT, o resto é EIO (o motivo
// fica no log da cola, não no errno, que não tem como levar texto).
func remoteErr(err error) error {
	if errors.Is(err, objectstore.ErrNotFound) {
		return syscall.ENOENT
	}
	return syscall.EIO
}

// Stat diz o que existe em p: arquivo (HEAD acha o objeto), diretório (há algo abaixo de
// p/, ou o marcador p/) ou ENOENT. Arquivo em escrita responde pela cópia local.
func (f *FS) Stat(ctx context.Context, p string) (Info, error) {
	p = strings.Trim(p, "/")
	name := path.Base(p)
	if p == "" {
		return Info{Name: "", Dir: true}, nil
	}
	if fi, ok := f.stagedInfo(p); ok {
		return fi, nil
	}
	f.mu.Lock()
	local := f.dirs[p]
	f.mu.Unlock()
	if local {
		return Info{Name: name, Dir: true}, nil
	}
	e, err := f.remote.Head(ctx, f.key(p))
	if err == nil {
		return Info{Name: name, Size: e.Size, ModTime: e.ModTime}, nil
	}
	if !errors.Is(err, objectstore.ErrNotFound) {
		return Info{}, syscall.EIO
	}
	es, err := f.remote.List(ctx, f.key(p)+"/")
	if err != nil {
		return Info{}, remoteErr(err)
	}
	if len(es) == 0 {
		return Info{}, syscall.ENOENT
	}
	return Info{Name: name, Dir: true}, nil
}

// stagedInfo devolve o tamanho da cópia local, se p está aberto para escrita.
func (f *FS) stagedInfo(p string) (Info, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	s, ok := f.stages[p]
	if !ok {
		return Info{}, false
	}
	st, err := s.file.Stat()
	if err != nil {
		return Info{}, false
	}
	return Info{Name: path.Base(p), Size: st.Size(), ModTime: st.ModTime()}, true
}

// ReadDir lista o que está logo abaixo de p. O marcador do próprio diretório (p/, criado
// pelo mkdir) não aparece; arquivo criado e ainda não enviado aparece.
func (f *FS) ReadDir(ctx context.Context, p string) ([]Info, error) {
	p = strings.Trim(p, "/")
	prefix := f.key(p)
	if p != "" {
		prefix += "/"
	}
	es, err := f.remote.List(ctx, prefix)
	if err != nil {
		return nil, remoteErr(err)
	}
	seen := map[string]bool{}
	var out []Info
	for _, e := range es {
		name := strings.TrimSuffix(strings.TrimPrefix(e.Key, prefix), "/")
		if name == "" || seen[name] {
			continue
		}
		seen[name] = true
		// Marcador de diretório pode voltar como objeto ("x/") em vez de prefixo comum,
		// conforme o servidor: a barra final decide, não só o IsDir.
		out = append(out, Info{Name: name, Size: e.Size, ModTime: e.ModTime, Dir: e.IsDir || strings.HasSuffix(e.Key, "/")})
	}
	f.mu.Lock()
	for sp := range f.stages {
		if n := path.Base(sp); parentOf(sp) == p && !seen[n] {
			seen[n] = true
			out = append(out, Info{Name: n})
		}
	}
	for d := range f.dirs {
		if n := path.Base(d); parentOf(d) == p && !seen[n] {
			seen[n] = true
			out = append(out, Info{Name: n, Dir: true})
		}
	}
	f.mu.Unlock()
	return out, nil
}

// ReadAt lê de p a partir de off. Arquivo em escrita lê da cópia local: quem lê vê o que foi
// escrito antes do flush.
func (f *FS) ReadAt(ctx context.Context, p string, buf []byte, off int64) (int, error) {
	p = strings.Trim(p, "/")
	f.mu.Lock()
	s, ok := f.stages[p]
	f.mu.Unlock()
	if ok {
		n, err := s.file.ReadAt(buf, off)
		if err == io.EOF {
			err = nil
		}
		return n, err
	}
	r, err := f.remote.Get(ctx, f.key(p), off, int64(len(buf)))
	if err != nil {
		// Leitura além do fim responde 416 no S3: para o kernel é só EOF.
		var e *objectstore.Error
		if errors.As(err, &e) && e.Status == 416 {
			return 0, nil
		}
		return 0, remoteErr(err)
	}
	defer r.Close()
	n, err := io.ReadFull(r, buf)
	if err == io.ErrUnexpectedEOF || err == io.EOF {
		err = nil
	}
	if err != nil {
		return n, syscall.EIO
	}
	return n, nil
}

// OpenWrite prepara p para escrita: na primeira abertura baixa o conteúdo atual (a menos que
// truncate, ou que o objeto não exista) para a cópia local. create marca a cópia como suja,
// para que um arquivo novo e vazio também suba no flush.
func (f *FS) OpenWrite(ctx context.Context, p string, truncate, create bool) error {
	if f.readOnly {
		return syscall.EROFS
	}
	p = strings.Trim(p, "/")
	f.mu.Lock()
	defer f.mu.Unlock()
	if s, ok := f.stages[p]; ok {
		s.refs++
		if truncate {
			s.file.Truncate(0)
			s.dirty = true
		}
		return nil
	}
	file, err := os.CreateTemp(f.stageDir, "s3stage-")
	if err != nil {
		return syscall.EIO
	}
	// O nome do arquivo local não importa: some já, e o descritor segue válido.
	os.Remove(file.Name())
	if !truncate {
		r, err := f.remote.Get(ctx, f.key(p), 0, 0)
		switch {
		case err == nil:
			_, err = io.Copy(file, r)
			r.Close()
			if err != nil {
				file.Close()
				return syscall.EIO
			}
		case errors.Is(err, objectstore.ErrNotFound):
			if !create {
				file.Close()
				return syscall.ENOENT
			}
		default:
			file.Close()
			return syscall.EIO
		}
	}
	f.stages[p] = &stage{file: file, refs: 1, dirty: truncate || create}
	return nil
}

// WriteAt escreve na cópia local de p (aberto por OpenWrite).
func (f *FS) WriteAt(p string, data []byte, off int64) (int, error) {
	s, err := f.staged(p)
	if err != nil {
		return 0, err
	}
	n, err := s.file.WriteAt(data, off)
	f.mu.Lock()
	s.dirty = true
	f.mu.Unlock()
	if err != nil {
		return n, syscall.EIO
	}
	return n, nil
}

// Truncate muda o tamanho de p. Fora de escrita, abre, trunca e sobe na hora (é o truncate(2)
// de um arquivo não aberto).
func (f *FS) Truncate(ctx context.Context, p string, size int64) error {
	p = strings.Trim(p, "/")
	if _, err := f.staged(p); err != nil {
		if err := f.OpenWrite(ctx, p, size == 0, false); err != nil {
			return err
		}
		defer f.Release(ctx, p)
	}
	s, _ := f.staged(p)
	if err := s.file.Truncate(size); err != nil {
		return syscall.EIO
	}
	f.mu.Lock()
	s.dirty = true
	f.mu.Unlock()
	return f.Flush(ctx, p)
}

// staged devolve a cópia local de p, ou EBADF se p não está aberto para escrita.
func (f *FS) staged(p string) (*stage, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	s, ok := f.stages[strings.Trim(p, "/")]
	if !ok {
		return nil, syscall.EBADF
	}
	return s, nil
}

// Flush sobe a cópia local de p se ela mudou desde o último envio. Falha no envio mantém a
// cópia suja: o próximo flush tenta de novo.
func (f *FS) Flush(ctx context.Context, p string) error {
	p = strings.Trim(p, "/")
	s, err := f.staged(p)
	if err != nil {
		return nil // aberto só para leitura: nada a enviar
	}
	f.mu.Lock()
	dirty := s.dirty
	f.mu.Unlock()
	if !dirty {
		return nil
	}
	// Leitura por SectionReader: não mexe no deslocamento do arquivo, que os WriteAt ignoram
	// mas um Seek do Put mudaria por baixo de quem estiver escrevendo.
	if err := f.remote.Put(ctx, f.key(p), io.NewSectionReader(s.file, 0, 1<<62)); err != nil {
		return syscall.EIO
	}
	f.mu.Lock()
	s.dirty = false
	f.mu.Unlock()
	return nil
}

// Release fecha um handle de escrita; o último sobe o que faltar e descarta a cópia local.
func (f *FS) Release(ctx context.Context, p string) error {
	p = strings.Trim(p, "/")
	s, err := f.staged(p)
	if err != nil {
		return nil
	}
	f.mu.Lock()
	s.refs--
	last := s.refs <= 0
	f.mu.Unlock()
	if !last {
		return nil
	}
	err = f.Flush(ctx, p)
	f.mu.Lock()
	delete(f.stages, p)
	f.mu.Unlock()
	s.file.Close()
	return err
}

// Remove apaga o arquivo p.
func (f *FS) Remove(ctx context.Context, p string) error {
	if f.readOnly {
		return syscall.EROFS
	}
	p = strings.Trim(p, "/")
	if err := f.remote.Delete(ctx, f.key(p)); err != nil {
		return remoteErr(err)
	}
	return nil
}

// parentOf é o diretório pai de p, com "" para a raiz.
func parentOf(p string) string {
	if d := path.Dir(p); d != "." {
		return d
	}
	return ""
}

// Mkdir cria o diretório nesta montagem, sem tocar no bucket (veja FS.dirs). Diretório vazio
// vive enquanto a montagem existir; com um arquivo dentro, existe no bucket.
func (f *FS) Mkdir(ctx context.Context, p string) error {
	if f.readOnly {
		return syscall.EROFS
	}
	p = strings.Trim(p, "/")
	if _, err := f.Stat(ctx, p); err == nil {
		return syscall.EEXIST
	} else if err != syscall.ENOENT {
		return err
	}
	f.mu.Lock()
	f.dirs[p] = true
	f.mu.Unlock()
	return nil
}

// Rmdir apaga o diretório p se ele está vazio (só o marcador, se tiver).
func (f *FS) Rmdir(ctx context.Context, p string) error {
	if f.readOnly {
		return syscall.EROFS
	}
	children, err := f.ReadDir(ctx, p)
	if err != nil {
		return err
	}
	if len(children) > 0 {
		return syscall.ENOTEMPTY
	}
	f.mu.Lock()
	delete(f.dirs, strings.Trim(p, "/"))
	f.mu.Unlock()
	// O marcador pode existir, criado por outra ferramenta (o console da AWS cria): sai junto.
	if err := f.remote.Delete(ctx, f.key(p)+"/"); err != nil {
		return remoteErr(err)
	}
	return nil
}

// Rename move o arquivo from para to: envia o que estiver pendente, copia no servidor e só
// então apaga a origem (cópia que falha deixa a origem intacta). Diretório não renomeia.
func (f *FS) Rename(ctx context.Context, from, to string) error {
	if f.readOnly {
		return syscall.EROFS
	}
	from, to = strings.Trim(from, "/"), strings.Trim(to, "/")
	fi, err := f.Stat(ctx, from)
	if err != nil {
		return err
	}
	if fi.Dir {
		return syscall.ENOTSUP
	}
	if err := f.Flush(ctx, from); err != nil {
		return err
	}
	if err := f.remote.Copy(ctx, f.key(from), f.key(to)); err != nil {
		return remoteErr(err)
	}
	if err := f.remote.Delete(ctx, f.key(from)); err != nil {
		return remoteErr(err)
	}
	f.mu.Lock()
	if s, ok := f.stages[from]; ok {
		delete(f.stages, from)
		f.stages[to] = s
	}
	f.mu.Unlock()
	return nil
}

// ReadOnly diz se o volume foi montado :ro.
func (f *FS) ReadOnly() bool { return f.readOnly }

// Prefix é o prefixo do bucket montado ("" ou terminado em /).
func (f *FS) Prefix() string { return f.prefix }
