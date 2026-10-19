// Package machinestore guarda as máquinas persistentes no disco do host: um diretório por
// máquina, com o registro (machine.json), o disco gravável (disk.ext4), a spec da VM, o
// socket do vsock, os logs e o pid do vmm.
package machinestore

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"syscall"

	"runner.code/internal/domain"
)

// Nomes dos arquivos dentro do diretório de uma máquina.
const (
	recordFile = "machine.json"
	DiskFile   = "disk.ext4"
	pidFile    = "vmm.pid"
)

// Store implementa ports.MachineStore sobre Root (<cache>/machines).
type Store struct {
	Root string
	// clone copia o modelo do disco; trocado no teste para simular falha.
	clone func(src, dst string) error
}

// New devolve o store em root.
func New(root string) *Store { return &Store{Root: root, clone: cloneFile} }

// Dir é o diretório da máquina.
func (s *Store) Dir(name string) string { return filepath.Join(s.Root, name) }

// Create cria o diretório (falha com CONFLICT se já existe), clona o disco e grava o
// registro por último: máquina sem machine.json não aparece no List, então uma criação
// interrompida no meio não deixa máquina pela metade visível.
func (s *Store) Create(m domain.Machine, diskTemplate string) error {
	if err := os.MkdirAll(s.Root, 0o700); err != nil {
		return err
	}
	dir := s.Dir(m.Name)
	if err := os.Mkdir(dir, 0o700); err != nil {
		if errors.Is(err, fs.ErrExist) {
			return domain.ErrMachineExists(m.Name)
		}
		return err
	}
	if err := s.clone(diskTemplate, filepath.Join(dir, DiskFile)); err != nil {
		os.RemoveAll(dir)
		return fmt.Errorf("clone machine disk: %w", err)
	}
	raw, _ := json.MarshalIndent(m, "", "  ")
	if err := os.WriteFile(filepath.Join(dir, recordFile), raw, 0o600); err != nil {
		os.RemoveAll(dir)
		return err
	}
	return nil
}

// Load lê o registro; ausente é NOT_FOUND.
func (s *Store) Load(name string) (domain.Machine, error) {
	var m domain.Machine
	raw, err := os.ReadFile(filepath.Join(s.Dir(name), recordFile))
	if errors.Is(err, fs.ErrNotExist) {
		return m, domain.ErrMachineNotFound(name)
	}
	if err != nil {
		return m, err
	}
	if err := json.Unmarshal(raw, &m); err != nil {
		return m, fmt.Errorf("machine %s: corrupt record: %w", name, err)
	}
	return m, nil
}

// List devolve as máquinas em ordem de nome; diretório sem registro é ignorado.
func (s *Store) List() ([]domain.Machine, error) {
	entries, err := os.ReadDir(s.Root)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var out []domain.Machine
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		if m, err := s.Load(e.Name()); err == nil {
			out = append(out, m)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Name < out[j].Name })
	return out, nil
}

// Delete apaga o diretório inteiro da máquina (disco incluído); ausente é NOT_FOUND.
func (s *Store) Delete(name string) error {
	if _, err := s.Load(name); err != nil {
		return err
	}
	return os.RemoveAll(s.Dir(name))
}

// PID devolve o pid registrado do vmm, ou 0.
func (s *Store) PID(name string) int {
	raw, err := os.ReadFile(filepath.Join(s.Dir(name), pidFile))
	if err != nil {
		return 0
	}
	pid, err := strconv.Atoi(strings.TrimSpace(string(raw)))
	if err != nil {
		return 0
	}
	return pid
}

// SetPID grava o pid; 0 apaga o registro (máquina parada).
func (s *Store) SetPID(name string, pid int) error {
	p := filepath.Join(s.Dir(name), pidFile)
	if pid == 0 {
		if err := os.Remove(p); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
		return nil
	}
	return os.WriteFile(p, []byte(strconv.Itoa(pid)), 0o600)
}

// Processes implementa ports.Processes com sinais do Unix.
type Processes struct{}

// Alive diz se o pid existe: kill com sinal 0 só confere. EPERM também é vivo (existe,
// mas é de outro usuário).
func (Processes) Alive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := syscall.Kill(pid, 0)
	return err == nil || errors.Is(err, syscall.EPERM)
}

// Kill manda SIGKILL ao grupo do vmm (ele lidera a própria sessão, então o grupo é o pid).
func (Processes) Kill(pid int) {
	if pid > 0 {
		syscall.Kill(-pid, syscall.SIGKILL)
	}
}
