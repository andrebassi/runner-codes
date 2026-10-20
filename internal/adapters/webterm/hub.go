package webterm

import (
	"context"
	"fmt"
	"sync"
	"time"

	"runner.code/internal/ports"
)

// maxSessions limita as abas de terminal de uma página: cada uma é um PTY e um processo no guest, e uma página não
// deveria poder abrir processos sem fim (o token já protege de terceiros; isto protege de um laço na própria página).
const maxSessions = 8

// maxEvents é quanto da linha do tempo a aba "eventos" guarda: o suficiente para uma sessão de trabalho, sem crescer.
const maxEvents = 200

// MainSession é o id da sessão aberta pelo comando (a primeira aba).
const MainSession = "main"

// Event é uma linha da aba "eventos": porta aberta ou fechada, sessão iniciada ou encerrada.
type Event struct {
	At   time.Time `json:"at"`
	Text string    `json:"text"`
}

// TabInfo é uma aba de terminal como a página a vê.
type TabInfo struct {
	ID    string `json:"id"`
	Label string `json:"label"`
	Ended bool   `json:"ended"`
	Code  int    `json:"code"`
}

// State é o que a página consulta para desenhar as abas: a máquina, os terminais, as portas e os eventos.
type State struct {
	Machine  string    `json:"machine"`
	Sessions []TabInfo `json:"sessions"`
	Ports    []Forward `json:"ports"`
	Events   []Event   `json:"events"`
}

// tab é uma sessão com o rótulo que a aba mostra.
type tab struct {
	label string
	sess  *Session
}

// Hub junta as sessões de terminal de uma página (a do comando e as de shell abertas pela aba "shell"), o
// encaminhamento de portas e a linha do tempo.
type Hub struct {
	token   string
	machine string
	ctx     context.Context
	// newShell abre um shell novo na máquina (aba "shell"); nil desliga a aba.
	newShell ShellFunc
	fw       *Forwarder

	mu     sync.Mutex
	order  []string
	tabs   map[string]*tab
	events []Event
	nextID int
}

// NewHub monta o hub com a sessão principal já ligada (mainLabel é o nome da aba, como "claude").
func NewHub(ctx context.Context, token, machine string, main *Session, mainLabel string, newShell ShellFunc, fw *Forwarder) *Hub {
	h := &Hub{token: token, machine: machine, ctx: ctx, newShell: newShell, fw: fw, tabs: map[string]*tab{}}
	h.add(MainSession, mainLabel, main)
	if fw != nil {
		fw.notify = h.Logf
	}
	return h
}

// Logf acrescenta uma linha à aba "eventos", descartando as mais velhas além de maxEvents.
func (h *Hub) Logf(format string, args ...any) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.events = append(h.events, Event{At: time.Now(), Text: fmt.Sprintf(format, args...)})
	if over := len(h.events) - maxEvents; over > 0 {
		h.events = append([]Event(nil), h.events[over:]...)
	}
}

// add registra a sessão e anota na linha do tempo quando ela começa e quando o programa sai.
func (h *Hub) add(id, label string, s *Session) {
	h.mu.Lock()
	h.tabs[id] = &tab{label: label, sess: s}
	h.order = append(h.order, id)
	h.mu.Unlock()
	h.Logf("%s: sessão iniciada", label)
	go func() {
		<-s.Done()
		h.Logf("%s: saiu com código %d", label, s.exitCode())
	}()
}

// session devolve a sessão do id ("" é a principal), ou nil.
func (h *Hub) session(id string) *Session {
	if id == "" {
		id = MainSession
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	if t, ok := h.tabs[id]; ok {
		return t.sess
	}
	return nil
}

// OpenShell abre uma aba de shell nova; erro se a aba estiver desligada ou o teto de sessões vivas tiver sido atingido.
func (h *Hub) OpenShell() (TabInfo, error) {
	if h.newShell == nil {
		return TabInfo{}, fmt.Errorf("shell tab disabled")
	}
	h.mu.Lock()
	alive := 0
	for _, t := range h.tabs {
		select {
		case <-t.sess.Done():
		default:
			alive++
		}
	}
	if alive >= maxSessions {
		h.mu.Unlock()
		return TabInfo{}, fmt.Errorf("too many sessions (%d)", maxSessions)
	}
	h.nextID++
	id := fmt.Sprintf("shell-%d", h.nextID)
	label := "shell"
	if h.nextID > 1 {
		label = fmt.Sprintf("shell %d", h.nextID)
	}
	h.mu.Unlock()
	h.add(id, label, Start(h.ctx, h.newShell, webDefaultSize))
	return TabInfo{ID: id, Label: label}, nil
}

// webDefaultSize é o tamanho de um shell novo até a aba mandar o dela (o primeiro resize corrige em milissegundos).
var webDefaultSize = ports.Winsize{Rows: 32, Cols: 120}

// Snapshot é o estado atual para a página.
func (h *Hub) Snapshot() State {
	st := State{Machine: h.machine, Ports: []Forward{}}
	if h.fw != nil {
		st.Ports = h.fw.Ports()
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	for _, id := range h.order {
		t := h.tabs[id]
		info := TabInfo{ID: id, Label: t.label}
		select {
		case <-t.sess.Done():
			info.Ended, info.Code = true, t.sess.exitCode()
		default:
		}
		st.Sessions = append(st.Sessions, info)
	}
	st.Events = append([]Event{}, h.events...)
	return st
}
