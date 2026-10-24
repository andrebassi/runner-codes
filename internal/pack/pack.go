// Package pack é o formato do artefato portátil (`rc pack create`): uma imagem pronta — e,
// vindo de uma máquina, o disco dela — num arquivo que se copia para outro host e roda sem
// pull nem build. Os discos ext4 são esparsos (um disco de 10 GiB com 100 MiB de dado), e o
// archive/tar do Go não grava esparso: o formato guarda só os extents com dado, achados por
// SEEK_DATA/SEEK_HOLE, e a extração recria o buraco.
//
// Layout: "RCPACK1\n" e um fluxo gzip com [uint32 BE tamanho do manifesto][manifesto JSON]
// e os bytes de cada extent, na ordem do manifesto.
package pack

import (
	"bufio"
	"compress/gzip"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"runner.codes/internal/domain"
)

// Magic abre todo payload.
const Magic = "RCPACK1\n"

// SidecarExt é a extensão do payload que viaja ao lado do stub.
const SidecarExt = ".rcmachine"

// trailerMagic fecha o arquivo único: os 8 bytes antes dele são o deslocamento do payload.
const trailerMagic = "RCPACKZ1"

// maxManifest limita o manifesto lido (um arquivo corrompido não aloca gigabytes).
const maxManifest = 1 << 20

// Version é a versão do formato; payload de versão maior é recusado com o motivo.
const Version = 1

// Manifest descreve o artefato.
type Manifest struct {
	Version      int            `json:"version"`
	Name         string         `json:"name"`
	Created      time.Time      `json:"created"`
	RCVersion    string         `json:"rc_version"`
	Platform     string         `json:"platform"`      // do guest: linux/arm64
	HostPlatform string         `json:"host_platform"` // do stub: darwin/arm64
	Source       string         `json:"source"`        // stack:python, image:alpine, machine:dev
	Machine      domain.Machine `json:"machine"`
	Files        []File         `json:"files"`
}

// File é um arquivo do payload: o tamanho lógico e os trechos com dado.
type File struct {
	Name    string    `json:"name"`
	Size    int64     `json:"size"`
	Extents []Extent  `json:"extents"`
	ModTime time.Time `json:"mod_time"`
}

// Extent é um trecho com dado: deslocamento e tamanho.
type Extent struct {
	Off int64 `json:"off"`
	Len int64 `json:"len"`
}

// Write grava o payload em w: o manifesto (com os extents de cada arquivo, achados agora) e
// os dados. paths são os arquivos, na ordem; o nome no payload é o basename.
func Write(w io.Writer, m Manifest, paths []string) error {
	m.Version = Version
	m.Files = nil
	for _, p := range paths {
		fi, err := os.Stat(p)
		if err != nil {
			return err
		}
		ext, err := extents(p, fi.Size())
		if err != nil {
			return fmt.Errorf("extents of %s: %w", p, err)
		}
		m.Files = append(m.Files, File{Name: filepath.Base(p), Size: fi.Size(), Extents: ext, ModTime: fi.ModTime()})
	}
	raw, _ := json.Marshal(m) // só tipos simples: não falha
	if _, err := io.WriteString(w, Magic); err != nil {
		return err
	}
	zw, _ := gzip.NewWriterLevel(w, gzip.BestSpeed) // nível válido: não falha
	var n [4]byte
	binary.BigEndian.PutUint32(n[:], uint32(len(raw)))
	zw.Write(n[:])
	zw.Write(raw)
	for i, f := range m.Files {
		if err := copyExtents(zw, paths[i], f.Extents); err != nil {
			return err
		}
	}
	return zw.Close()
}

// copyExtents escreve em w os bytes de cada extent do arquivo.
func copyExtents(w io.Writer, path string, ext []Extent) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	for _, e := range ext {
		if _, err := io.Copy(w, io.NewSectionReader(f, e.Off, e.Len)); err != nil {
			return err
		}
	}
	return nil
}

// ReadManifest lê só o manifesto (para `pack inspect` e para decidir antes de extrair).
func ReadManifest(r io.Reader) (Manifest, *bufio.Reader, error) {
	var m Manifest
	magic := make([]byte, len(Magic))
	if _, err := io.ReadFull(r, magic); err != nil || string(magic) != Magic {
		return m, nil, errors.New("not a runner.code pack")
	}
	zr, err := gzip.NewReader(r)
	if err != nil {
		return m, nil, fmt.Errorf("pack: %w", err)
	}
	// Um membro só: no arquivo único o trailer vem depois do gzip, e o leitor multistream
	// tentaria lê-lo como um segundo membro (medido: "gzip: invalid header" no último arquivo).
	zr.Multistream(false)
	br := bufio.NewReader(zr)
	var n [4]byte
	if _, err := io.ReadFull(br, n[:]); err != nil {
		return m, nil, fmt.Errorf("pack manifest: %w", err)
	}
	size := binary.BigEndian.Uint32(n[:])
	if size > maxManifest {
		return m, nil, errors.New("pack manifest is too large")
	}
	raw := make([]byte, size)
	if _, err := io.ReadFull(br, raw); err != nil {
		return m, nil, fmt.Errorf("pack manifest: %w", err)
	}
	if err := json.Unmarshal(raw, &m); err != nil {
		return m, nil, fmt.Errorf("pack manifest: %w", err)
	}
	if m.Version > Version {
		return m, nil, fmt.Errorf("pack version %d is newer than this rc understands (%d)", m.Version, Version)
	}
	return m, br, nil
}

// Extract lê o payload e recria cada arquivo em dir, esparso (o tamanho lógico com buracos
// onde não havia dado). Nome com barra ou ".." é recusado: o payload vem de fora.
func Extract(r io.Reader, dir string) (Manifest, error) {
	m, br, err := ReadManifest(r)
	if err != nil {
		return m, err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return m, err
	}
	for _, f := range m.Files {
		if f.Name == "" || f.Name == "." || f.Name == ".." || strings.ContainsAny(f.Name, `/\`) {
			return m, fmt.Errorf("pack file name %q is not allowed", f.Name)
		}
		if err := writeSparse(br, filepath.Join(dir, f.Name), f); err != nil {
			return m, fmt.Errorf("pack file %s: %w", f.Name, err)
		}
	}
	return m, nil
}

// writeSparse cria o arquivo com o tamanho lógico e escreve só os extents.
func writeSparse(r io.Reader, path string, f File) error {
	out, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return err
	}
	defer out.Close()
	if err := out.Truncate(f.Size); err != nil {
		return err
	}
	for _, e := range f.Extents {
		if e.Off < 0 || e.Len < 0 || e.Off+e.Len > f.Size {
			return errors.New("extent outside the file")
		}
		n, err := io.Copy(io.NewOffsetWriter(out, e.Off), io.LimitReader(r, e.Len))
		if err != nil {
			return err
		}
		if n != e.Len {
			return io.ErrUnexpectedEOF // payload cortado: o arquivo sairia com buraco onde havia dado
		}
	}
	return out.Close()
}

// Locate acha o payload de um executável: o sidecar <exe>.rcmachine ao lado, ou o payload
// anexado ao próprio executável (arquivo único). Devolve o arquivo aberto, já posicionado no
// início do payload, e ok=false se não houver payload (o rc comum).
func Locate(exe string) (*os.File, bool, error) {
	if f, err := os.Open(exe + SidecarExt); err == nil {
		return f, true, nil
	}
	f, err := os.Open(exe)
	if err != nil {
		return nil, false, err
	}
	fi, err := f.Stat()
	if err != nil || fi.Size() < 16 {
		f.Close()
		return nil, false, err
	}
	var tr [16]byte
	if _, err := f.ReadAt(tr[:], fi.Size()-16); err != nil || string(tr[8:]) != trailerMagic {
		f.Close()
		return nil, false, nil
	}
	off := int64(binary.BigEndian.Uint64(tr[:8]))
	if off <= 0 || off >= fi.Size()-16 {
		f.Close()
		return nil, false, errors.New("pack trailer points outside the executable")
	}
	if _, err := f.Seek(off, io.SeekStart); err != nil {
		f.Close()
		return nil, false, err
	}
	return f, true, nil
}

// AppendTo anexa o payload ao stub e fecha com o trailer (arquivo único).
func AppendTo(stub string, payload io.Reader) error {
	f, err := os.OpenFile(stub, os.O_WRONLY|os.O_APPEND, 0)
	if err != nil {
		return err
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil {
		return err
	}
	if _, err := io.Copy(f, payload); err != nil {
		return err
	}
	var tr [16]byte
	binary.BigEndian.PutUint64(tr[:8], uint64(fi.Size()))
	copy(tr[8:], trailerMagic)
	if _, err := f.Write(tr[:]); err != nil {
		return err
	}
	return f.Close()
}

// Key é a identidade do payload no cache: a impressão do manifesto (que traz os extents e a
// data do pack), curta o bastante para nome de diretório.
func Key(m Manifest) string {
	raw, _ := json.Marshal(m) // só tipos simples: não falha
	sum := sha256.Sum256(raw)
	return hex.EncodeToString(sum[:6])
}

// Portable tira da máquina o que não viaja para outro host e recusa o que não faria sentido
// lá: diretório do host montado (o caminho é de outra máquina) e credenciais (CA e
// placeholders são da máquina de origem). Rede nomeada e data de criação saem.
func Portable(mc domain.Machine) (domain.Machine, error) {
	for _, mt := range mc.Mounts {
		if _, _, ok := mt.S3(); !ok && !mt.Disk() {
			return mc, fmt.Errorf("volume %s is a host directory and does not travel in a pack (s3:// volumes do)", mt)
		}
	}
	if len(mc.Network.Credentials) > 0 {
		return mc, errors.New("credentials do not travel in a pack: declare them on the machine created from it")
	}
	mc.Network.Name, mc.Network.Address = "", ""
	mc.CreatedAt = time.Time{}
	return mc, nil
}

// Route traduz a linha de comando de um artefato para os subcomandos do rc: run é uma máquina
// efêmera do pack; start cria a máquina persistente na primeira vez; exec, stop, status, shell
// e logs vão a ela; inspect mostra o manifesto. Sem subcomando conhecido, o erro é o uso.
func Route(m Manifest, exe string, args []string) ([]string, error) {
	sub, rest := "", []string(nil)
	if len(args) > 0 {
		sub, rest = args[0], args[1:]
	}
	switch sub {
	case "run":
		return append([]string{"machine", "run", "--from", exe}, rest...), nil
	case "start":
		return append([]string{"machine", "start", "--from", exe, "--name", m.Name}, rest...), nil
	case "exec", "stop", "status", "shell", "logs":
		return append([]string{"machine", sub, "--name", m.Name}, rest...), nil
	case "inspect":
		return []string{"pack", "inspect", exe}, nil
	}
	base := filepath.Base(exe)
	return nil, fmt.Errorf("%s: packed %s (%s, %s)\nusage: %s run [-- COMMAND...] | start | exec -- COMMAND... | shell | logs | status | stop | inspect",
		base, m.Name, m.Source, m.Platform, base)
}
