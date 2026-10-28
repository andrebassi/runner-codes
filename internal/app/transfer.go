package app

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/rand"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"runner.codes/internal/domain"
)

// Formato da exportação de uma máquina (machine export/import), dentro de um gzip:
//
//	"RCMX1\n"
//	por arquivo: 'F' uint16(len nome) nome uint64(tamanho), depois blocos até somar o tamanho:
//	             'D' uint32(n) n bytes  |  'Z' uint32(n)  (n bytes zerados, sem conteúdo)
//	'E' no fim
//
// O bloco de zeros é o que faz a mudança caber: o disco da máquina é esparso (12 GiB declarados,
// ~1 GiB gravado), tar não sabe escrever buraco e copiar os zeros multiplicaria o tráfego. Na
// importação o bloco Z vira Seek, e o arquivo volta esparso.
const transferMagic = "RCMX1\n"

// transferChunk é o tamanho de cada bloco lido do arquivo (e o teto de um bloco D na importação).
const transferChunk = 1 << 20

// transferMaxFile é o teto de um arquivo na importação: corpo malicioso não declara um arquivo de
// exabytes para encher o disco do host.
const transferMaxFile = 4 << 40

// skipOnExport são os arquivos de uma máquina que não viajam: o pid e os sockets são da VM que
// rodava (o rc recria no próximo start), os logs ficam com o servidor de origem.
func skipOnExport(name string) bool {
	return name == pidFileName || name == machinePaused || strings.HasSuffix(name, ".sock") || strings.HasSuffix(name, ".log")
}

// pidFileName é o pid do vmm no diretório da máquina (machinestore.pidFile).
const pidFileName = "vmm.pid"

// recordFileName é o registro da máquina (machinestore.recordFile): é ele que decide o nome.
const recordFileName = "machine.json"

// Export escreve a máquina desligada em w no formato acima. Máquina ligada ou pausada é recusada:
// o disco mudaria no meio da cópia e o destino receberia um ext4 rasgado.
func (m *Machines) Export(ctx context.Context, name string, w io.Writer) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State != StateStopped {
		return domain.ErrMachineState(name, "must be stopped to export")
	}
	dir := m.Store.Dir(name)
	entries, err := os.ReadDir(dir)
	if err != nil {
		return err
	}
	var files []string
	for _, e := range entries {
		if e.Type().IsRegular() && !skipOnExport(e.Name()) {
			files = append(files, e.Name())
		}
	}
	// O registro vai por último: quem importa só cria a máquina quando tudo chegou.
	sort.Slice(files, func(i, j int) bool {
		if files[i] == recordFileName || files[j] == recordFileName {
			return files[j] == recordFileName && files[i] != recordFileName
		}
		return files[i] < files[j]
	})
	zw, _ := gzip.NewWriterLevel(w, gzip.BestSpeed) // nível constante e válido: não falha
	bw := bufio.NewWriterSize(zw, 1<<20)
	if _, err := bw.WriteString(transferMagic); err != nil {
		return err
	}
	for _, f := range files {
		if err := ctx.Err(); err != nil {
			return err
		}
		if err := writeFileEntry(bw, filepath.Join(dir, f), f); err != nil {
			return fmt.Errorf("export %s: %w", f, err)
		}
	}
	if err := bw.WriteByte('E'); err != nil {
		return err
	}
	if err := bw.Flush(); err != nil {
		return err
	}
	return zw.Close()
}

// zeros é um bloco zerado para comparar com o lido.
var zeros = make([]byte, transferChunk)

// writeFileEntry escreve um arquivo: cabeçalho e blocos, com os trechos zerados como bloco Z.
func writeFileEntry(w *bufio.Writer, path, name string) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return err
	}
	head := make([]byte, 0, 11+len(name))
	head = append(head, 'F')
	head = binary.BigEndian.AppendUint16(head, uint16(len(name)))
	head = append(head, name...)
	head = binary.BigEndian.AppendUint64(head, uint64(info.Size()))
	if _, err := w.Write(head); err != nil {
		return err
	}
	buf := make([]byte, transferChunk)
	for left := info.Size(); left > 0; {
		n := int(min(int64(transferChunk), left))
		if _, err := io.ReadFull(f, buf[:n]); err != nil {
			return err
		}
		kind := byte('D')
		if bytes.Equal(buf[:n], zeros[:n]) {
			kind = 'Z'
		}
		var hdr [5]byte
		hdr[0] = kind
		binary.BigEndian.PutUint32(hdr[1:], uint32(n))
		if _, err := w.Write(hdr[:]); err != nil {
			return err
		}
		if kind == 'D' {
			if _, err := w.Write(buf[:n]); err != nil {
				return err
			}
		}
		left -= int64(n)
	}
	return nil
}

// ErrBadExport é o corpo de importação que não está no formato (ou chegou cortado).
var ErrBadExport = errors.New("invalid machine export")

// badExport embrulha o defeito como validação (400 na API), com o motivo.
func badExport(msg string) error {
	return domain.ValidationError{Err: fmt.Errorf("%w: %s", ErrBadExport, msg)}
}

// Import lê uma exportação e cria a máquina com o nome que está no registro dela. Grava num
// diretório temporário ao lado e só no fim o renomeia para o da máquina: importação cortada no meio
// não deixa máquina pela metade, e nome que já existe é recusado (CONFLICT) sem tocar na existente.
func (m *Machines) Import(ctx context.Context, r io.Reader) (domain.Machine, error) {
	zr, err := gzip.NewReader(r)
	if err != nil {
		return domain.Machine{}, badExport("not gzip")
	}
	br := bufio.NewReaderSize(zr, 1<<20)
	magic := make([]byte, len(transferMagic))
	if _, err := io.ReadFull(br, magic); err != nil || string(magic) != transferMagic {
		return domain.Machine{}, badExport("missing header")
	}
	// O temporário mora no mesmo diretório das máquinas (mesmo sistema de arquivos: o rename final
	// é atômico) e começa com ponto, que o List do store ignora por não ter machine.json.
	root := filepath.Dir(m.Store.Dir("x"))
	if err := os.MkdirAll(root, 0o700); err != nil {
		return domain.Machine{}, err
	}
	var rnd [6]byte
	_, _ = rand.Read(rnd[:]) // crypto/rand não falha no Linux nem no macOS
	tmp := filepath.Join(root, ".import-"+hex.EncodeToString(rnd[:]))
	if err := os.Mkdir(tmp, 0o700); err != nil {
		return domain.Machine{}, err
	}
	done := false
	defer func() {
		if !done {
			os.RemoveAll(tmp)
		}
	}()
	for {
		if err := ctx.Err(); err != nil {
			return domain.Machine{}, err
		}
		kind, err := br.ReadByte()
		if err != nil {
			return domain.Machine{}, badExport("truncated")
		}
		if kind == 'E' {
			break
		}
		if kind != 'F' {
			return domain.Machine{}, badExport("unknown entry")
		}
		if err := readFileEntry(br, tmp); err != nil {
			return domain.Machine{}, err
		}
	}
	raw, err := os.ReadFile(filepath.Join(tmp, recordFileName))
	if err != nil {
		return domain.Machine{}, badExport("no machine.json")
	}
	var mc domain.Machine
	if err := json.Unmarshal(raw, &mc); err != nil {
		return domain.Machine{}, badExport("machine.json is not valid")
	}
	if err := domain.ValidateName(mc.Name); err != nil {
		return domain.Machine{}, err
	}
	if _, err := m.Store.Load(mc.Name); err == nil {
		return domain.Machine{}, domain.ErrMachineExists(mc.Name)
	}
	if m.Persist != nil {
		return m.importPersistent(ctx, mc, tmp)
	}
	if err := os.Rename(tmp, m.Store.Dir(mc.Name)); err != nil {
		if errors.Is(err, os.ErrExist) {
			return domain.Machine{}, domain.ErrMachineExists(mc.Name)
		}
		return domain.Machine{}, err
	}
	done = true
	return m.Store.Load(mc.Name)
}

// importPersistent termina o import num host com disco próprio por máquina: a máquina que chega por cópia (de um host
// sem o recurso, como a casa) também passa a morar num disco só dela — senão ela ficaria no disco do host e morreria
// com ele numa preempção. O disco nasce agora; disco que já existia é outra máquina com o mesmo nome, e o import recua
// (soltando o que adotou) com CONFLICT. Os arquivos vão do temporário para dentro do disco com os buracos preservados.
func (m *Machines) importPersistent(ctx context.Context, mc domain.Machine, tmp string) (domain.Machine, error) {
	dir := m.Store.Dir(mc.Name)
	created, err := m.Persist.Ensure(ctx, mc.Name, dir, persistentGiB(mc.DiskGiB))
	if err != nil {
		return domain.Machine{}, fmt.Errorf("persistent disk: %w", err)
	}
	if !created {
		m.Persist.Release(context.WithoutCancel(ctx), mc.Name, dir)
		return domain.Machine{}, domain.ErrMachineExists(mc.Name)
	}
	entries, err := os.ReadDir(tmp)
	if err == nil {
		for _, e := range entries {
			// O registro por último: sem machine.json a máquina não aparece no List, então um import cortado no meio
			// não deixa máquina pela metade visível.
			if e.Name() == recordFileName {
				continue
			}
			if err = copySparse(filepath.Join(tmp, e.Name()), filepath.Join(dir, e.Name())); err != nil {
				break
			}
		}
	}
	if err == nil {
		err = copySparse(filepath.Join(tmp, recordFileName), filepath.Join(dir, recordFileName))
	}
	if err != nil {
		m.Persist.Destroy(context.WithoutCancel(ctx), mc.Name, dir)
		return domain.Machine{}, fmt.Errorf("copy into persistent disk: %w", err)
	}
	return m.Store.Load(mc.Name)
}

// copySparse copia um arquivo deixando os blocos de zeros como buraco (Seek em vez de Write): o disco da máquina é
// esparso — 12 GiB declarados, ~1 GiB gravado —, e copiar os zeros gravaria os 12 GiB inteiros no disco de destino.
func copySparse(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	fi, err := in.Stat()
	if err != nil {
		return err
	}
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, fi.Mode().Perm())
	if err != nil {
		return err
	}
	buf := make([]byte, transferChunk)
	for {
		n, rerr := io.ReadFull(in, buf)
		if n > 0 {
			if bytes.Equal(buf[:n], make([]byte, n)) {
				_, err = out.Seek(int64(n), io.SeekCurrent)
			} else {
				_, err = out.Write(buf[:n])
			}
			if err != nil {
				out.Close()
				return err
			}
		}
		if rerr == io.EOF || rerr == io.ErrUnexpectedEOF {
			break
		}
		if rerr != nil {
			out.Close()
			return rerr
		}
	}
	// O Truncate fecha o tamanho: um arquivo que termina em buraco só "existe" até o último Write sem ele.
	if err := out.Truncate(fi.Size()); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}

// readFileEntry lê um arquivo da exportação para dentro de dir, com os blocos Z como buraco.
func readFileEntry(br *bufio.Reader, dir string) error {
	var nl [2]byte
	if _, err := io.ReadFull(br, nl[:]); err != nil {
		return badExport("truncated")
	}
	name := make([]byte, binary.BigEndian.Uint16(nl[:]))
	if _, err := io.ReadFull(br, name); err != nil {
		return badExport("truncated")
	}
	// Nome simples, sem barra nem ponto na frente: o corpo não escreve fora do diretório da máquina.
	if n := string(name); n == "" || strings.ContainsAny(n, `/\`) || strings.HasPrefix(n, ".") {
		return badExport("bad file name")
	}
	var sz [8]byte
	if _, err := io.ReadFull(br, sz[:]); err != nil {
		return badExport("truncated")
	}
	size := int64(binary.BigEndian.Uint64(sz[:]))
	if size < 0 || size > transferMaxFile {
		return badExport("file too large")
	}
	f, err := os.OpenFile(filepath.Join(dir, string(name)), os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600)
	if err != nil {
		return badExport("repeated file")
	}
	defer f.Close()
	buf := make([]byte, transferChunk)
	for got := int64(0); got < size; {
		var hdr [5]byte
		if _, err := io.ReadFull(br, hdr[:]); err != nil {
			return badExport("truncated")
		}
		n := int64(binary.BigEndian.Uint32(hdr[1:]))
		if n == 0 || n > transferChunk || got+n > size {
			return badExport("bad block")
		}
		switch hdr[0] {
		case 'D':
			if _, err := io.ReadFull(br, buf[:n]); err != nil {
				return badExport("truncated")
			}
			if _, err := f.Write(buf[:n]); err != nil {
				return err
			}
		case 'Z':
			if _, err := f.Seek(n, io.SeekCurrent); err != nil {
				return err
			}
		default:
			return badExport("bad block")
		}
		got += n
	}
	// Truncate fecha o tamanho mesmo quando o arquivo termina num buraco (Seek não estende).
	return f.Truncate(size)
}
