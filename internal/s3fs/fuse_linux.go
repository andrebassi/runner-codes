//go:build linux

package s3fs

import (
	"context"
	"path"
	"syscall"
	"time"

	"github.com/hanwen/go-fuse/v2/fs"
	"github.com/hanwen/go-fuse/v2/fuse"
)

// Cola FUSE: cada método só traduz a chamada do kernel para o núcleo (FS) e o erro para
// errno. Nenhuma decisão mora aqui — elas estão no s3fs.go, testadas no host; esta parte é
// provada pelo e2e com um MinIO de verdade.

// node é um arquivo ou diretório do volume; o caminho vem da árvore de inodes (Path), e
// não de um campo, para continuar certo depois de um rename.
type node struct {
	fs.Inode
	core *FS
}

// writeHandle marca um handle aberto para escrita (o flush e o release vão ao núcleo).
type writeHandle struct{}

// readHandle marca um handle só de leitura.
type readHandle struct{}

// cacheTimeout é quanto o kernel guarda nome e atributo: 1 s deixa um ls repetido barato e
// mantém a mudança feita por outro cliente do bucket visível logo.
const cacheTimeout = time.Second

// Mount monta core em dir e devolve o servidor (Unmount desmonta). DirectMount chama o
// mount(2) direto: a imagem do usuário não precisa ter fusermount.
func Mount(dir string, core *FS) (*fuse.Server, error) {
	t := cacheTimeout
	opts := &fs.Options{EntryTimeout: &t, AttrTimeout: &t, MountOptions: fuse.MountOptions{
		FsName: "s3", Name: "s3", DirectMount: true, DirectMountStrict: true, AllowOther: true}}
	if core.ReadOnly() {
		opts.MountOptions.Options = []string{"ro"}
	}
	return fs.Mount(dir, &node{core: core}, opts)
}

// errno leva o erro do núcleo ao kernel: errno passa como veio, o resto é EIO.
func errno(err error) syscall.Errno {
	if err == nil {
		return fs.OK
	}
	if e, ok := err.(syscall.Errno); ok {
		return e
	}
	return syscall.EIO
}

// rel é o caminho do nó relativo à raiz do volume.
func (n *node) rel() string { return n.Path(nil) }

// fill preenche o atributo a partir do Info do núcleo: dono root, modo fixo (o S3 não guarda
// permissão), 0444/0555 no :ro.
func (n *node) fill(i Info, a *fuse.Attr) {
	if i.Dir {
		a.Mode = syscall.S_IFDIR | 0o755
	} else {
		a.Mode = syscall.S_IFREG | 0o644
		a.Size = uint64(i.Size)
	}
	if n.core.ReadOnly() {
		a.Mode &^= 0o222
	}
	if !i.ModTime.IsZero() {
		a.SetTimes(nil, &i.ModTime, &i.ModTime)
	}
}

// child cria o inode de um filho.
func (n *node) child(ctx context.Context, i Info) *fs.Inode {
	mode := uint32(syscall.S_IFREG)
	if i.Dir {
		mode = syscall.S_IFDIR
	}
	return n.NewInode(ctx, &node{core: n.core}, fs.StableAttr{Mode: mode})
}

// Lookup acha name dentro do diretório.
func (n *node) Lookup(ctx context.Context, name string, out *fuse.EntryOut) (*fs.Inode, syscall.Errno) {
	i, err := n.core.Stat(ctx, path.Join(n.rel(), name))
	if err != nil {
		return nil, errno(err)
	}
	n.fill(i, &out.Attr)
	return n.child(ctx, i), fs.OK
}

// Readdir lista o diretório.
func (n *node) Readdir(ctx context.Context) (fs.DirStream, syscall.Errno) {
	is, err := n.core.ReadDir(ctx, n.rel())
	if err != nil {
		return nil, errno(err)
	}
	out := make([]fuse.DirEntry, 0, len(is))
	for _, i := range is {
		mode := uint32(syscall.S_IFREG)
		if i.Dir {
			mode = syscall.S_IFDIR
		}
		out = append(out, fuse.DirEntry{Name: i.Name, Mode: mode})
	}
	return fs.NewListDirStream(out), fs.OK
}

// Getattr devolve tamanho e data.
func (n *node) Getattr(ctx context.Context, _ fs.FileHandle, out *fuse.AttrOut) syscall.Errno {
	i, err := n.core.Stat(ctx, n.rel())
	if err != nil {
		return errno(err)
	}
	n.fill(i, &out.Attr)
	return fs.OK
}

// Setattr só leva o tamanho (truncate); dono, modo e data não persistem no S3.
func (n *node) Setattr(ctx context.Context, fh fs.FileHandle, in *fuse.SetAttrIn, out *fuse.AttrOut) syscall.Errno {
	if size, ok := in.GetSize(); ok {
		if err := n.core.Truncate(ctx, n.rel(), int64(size)); err != nil {
			return errno(err)
		}
	}
	return n.Getattr(ctx, fh, out)
}

// Open abre para leitura ou para escrita (O_TRUNC trunca).
func (n *node) Open(ctx context.Context, flags uint32) (fs.FileHandle, uint32, syscall.Errno) {
	if flags&(syscall.O_WRONLY|syscall.O_RDWR) == 0 {
		return readHandle{}, 0, fs.OK
	}
	if err := n.core.OpenWrite(ctx, n.rel(), flags&syscall.O_TRUNC != 0, false); err != nil {
		return nil, 0, errno(err)
	}
	return writeHandle{}, 0, fs.OK
}

// Create cria e abre um arquivo novo.
func (n *node) Create(ctx context.Context, name string, flags, mode uint32, out *fuse.EntryOut) (*fs.Inode, fs.FileHandle, uint32, syscall.Errno) {
	p := path.Join(n.rel(), name)
	if err := n.core.OpenWrite(ctx, p, true, true); err != nil {
		return nil, nil, 0, errno(err)
	}
	i := Info{Name: name}
	n.fill(i, &out.Attr)
	return n.child(ctx, i), writeHandle{}, 0, fs.OK
}

// Read lê do arquivo.
func (n *node) Read(ctx context.Context, _ fs.FileHandle, dest []byte, off int64) (fuse.ReadResult, syscall.Errno) {
	k, err := n.core.ReadAt(ctx, n.rel(), dest, off)
	if err != nil {
		return nil, errno(err)
	}
	return fuse.ReadResultData(dest[:k]), fs.OK
}

// Write escreve na cópia local.
func (n *node) Write(ctx context.Context, _ fs.FileHandle, data []byte, off int64) (uint32, syscall.Errno) {
	k, err := n.core.WriteAt(n.rel(), data, off)
	return uint32(k), errno(err)
}

// Flush sobe a escrita (close de cada descritor).
func (n *node) Flush(ctx context.Context, fh fs.FileHandle) syscall.Errno {
	if _, ok := fh.(writeHandle); !ok {
		return fs.OK
	}
	return errno(n.core.Flush(ctx, n.rel()))
}

// Fsync sobe a escrita.
func (n *node) Fsync(ctx context.Context, fh fs.FileHandle, _ uint32) syscall.Errno {
	return n.Flush(ctx, fh)
}

// Release fecha o handle; o último de um caminho descarta a cópia local.
func (n *node) Release(ctx context.Context, fh fs.FileHandle) syscall.Errno {
	if _, ok := fh.(writeHandle); !ok {
		return fs.OK
	}
	return errno(n.core.Release(ctx, n.rel()))
}

// Mkdir cria o diretório (marcador).
func (n *node) Mkdir(ctx context.Context, name string, _ uint32, out *fuse.EntryOut) (*fs.Inode, syscall.Errno) {
	if err := n.core.Mkdir(ctx, path.Join(n.rel(), name)); err != nil {
		return nil, errno(err)
	}
	i := Info{Name: name, Dir: true}
	n.fill(i, &out.Attr)
	return n.child(ctx, i), fs.OK
}

// Unlink apaga o arquivo.
func (n *node) Unlink(ctx context.Context, name string) syscall.Errno {
	return errno(n.core.Remove(ctx, path.Join(n.rel(), name)))
}

// Rmdir apaga o diretório vazio.
func (n *node) Rmdir(ctx context.Context, name string) syscall.Errno {
	return errno(n.core.Rmdir(ctx, path.Join(n.rel(), name)))
}

// Rename move um arquivo (diretório devolve ENOTSUP).
func (n *node) Rename(ctx context.Context, name string, newParent fs.InodeEmbedder, newName string, _ uint32) syscall.Errno {
	to := path.Join(newParent.EmbeddedInode().Path(nil), newName)
	return errno(n.core.Rename(ctx, path.Join(n.rel(), name), to))
}

// Interfaces que o go-fuse procura no nó: conferidas em compilação.
var (
	_ fs.NodeLookuper  = (*node)(nil)
	_ fs.NodeReaddirer = (*node)(nil)
	_ fs.NodeGetattrer = (*node)(nil)
	_ fs.NodeSetattrer = (*node)(nil)
	_ fs.NodeOpener    = (*node)(nil)
	_ fs.NodeCreater   = (*node)(nil)
	_ fs.NodeReader    = (*node)(nil)
	_ fs.NodeWriter    = (*node)(nil)
	_ fs.NodeFlusher   = (*node)(nil)
	_ fs.NodeFsyncer   = (*node)(nil)
	_ fs.NodeReleaser  = (*node)(nil)
	_ fs.NodeMkdirer   = (*node)(nil)
	_ fs.NodeUnlinker  = (*node)(nil)
	_ fs.NodeRmdirer   = (*node)(nil)
	_ fs.NodeRenamer   = (*node)(nil)
)
