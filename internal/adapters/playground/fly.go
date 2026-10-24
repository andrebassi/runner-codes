package playground

import (
	"context"
	"errors"
	"net"
	"net/http"
	"net/http/httputil"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"runner.codes/internal/adapters/flymachines"
	"runner.codes/internal/domain"
)

// ErrFull é a recusa quando todas as máquinas de visitante do teto estão em uso.
var ErrFull = errors.New("playground is full: every machine is in use, try again in a few minutes")

// FlyAPI é o que o FlyBackend usa da API de Machines (flymachines.Client; um fake no teste).
type FlyAPI interface {
	List(ctx context.Context) ([]flymachines.Machine, error)
	Create(ctx context.Context, name string, env, metadata map[string]string) (flymachines.Machine, error)
	Start(ctx context.Context, id string) error
	Stop(ctx context.Context, id string) error
	Wait(ctx context.Context, id, state string, timeout int) error
	Delete(ctx context.Context, m flymachines.Machine) error
}

// Portas do rc-visitor dentro da Fly Machine, só na rede privada do app (a máquina não tem serviço público).
const (
	VisitorTerminalPort = 7681
	VisitorPortsPort    = 7682
)

// Cabeçalhos da ponte de portas: o rc-visitor só atende com o token da máquina e tira os dois antes de repassar à app.
const (
	HeaderPort  = "X-Rc-Port"
	HeaderToken = "X-Rc-Token"
)

// FlyBackend dá a cada visitante uma Fly Machine própria (o Fly não expõe /dev/kvm, então o rc não roda lá: a Fly
// Machine já é a microVM). Teto de Max máquinas no app; quando cheio, apaga a parada há mais tempo para abrir vaga.
type FlyBackend struct {
	API    FlyAPI
	Secret []byte
	// Max é o teto de máquinas de visitante no app (ligadas ou paradas).
	Max int
	// IdleStop desliga a máquina sem uso há esse tempo; MaxAge apaga a que ficou esse tempo sem uso.
	IdleStop, MaxAge time.Duration
	// Host e PortDomain vão para o rc-visitor: o terminal aceita o Host público e mostra as portas no subdomínio.
	Host, PortDomain string
	Now              func() time.Time
	// Dial disca o rc-visitor (net.Dialer no Fly; trocado no teste).
	Dial func(ctx context.Context, addr string) (net.Conn, error)
	// ReadyTimeout é quanto o terminal espera o rc-visitor de uma máquina recém-ligada aceitar conexão.
	ReadyTimeout time.Duration

	mu      sync.Mutex
	lastUse map[string]time.Time
	ips     map[string]string
}

// Ensure devolve a máquina do visitante ligada: liga a parada, cria a nova (abrindo vaga se preciso) e espera o
// estado started. Serializado: duas abas do mesmo visitante não criam duas máquinas.
func (b *FlyBackend) Ensure(ctx context.Context, visitor string) (string, error) {
	name, err := domain.VisitorMachineName(visitor)
	if err != nil {
		return "", err
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	all, err := b.API.List(ctx)
	if err != nil {
		return "", err
	}
	m, found := find(all, name)
	if found && m.State == "failed" {
		// Máquina quebrada não volta com start: apaga e recria do zero (o volume vai junto).
		if err := b.API.Delete(ctx, m); err != nil {
			return "", err
		}
		all, found = remove(all, m.ID), false
	}
	if !found {
		if m, err = b.create(ctx, all, name); err != nil {
			return "", err
		}
	} else if m.State != "started" {
		if m.State != "starting" {
			if err := b.API.Start(ctx, m.ID); err != nil {
				return "", err
			}
		}
	}
	// 60 s é o máximo que a API aceita num wait; uma máquina criada com imagem em cache sobe em poucos segundos.
	if err := b.API.Wait(ctx, m.ID, "started", 60); err != nil {
		return "", err
	}
	b.remember(name, m.PrivateIP)
	return name, nil
}

// create abre vaga se o teto foi atingido (apagando a parada usada há mais tempo) e cria a máquina do visitante.
func (b *FlyBackend) create(ctx context.Context, all []flymachines.Machine, name string) (flymachines.Machine, error) {
	visitors := b.visitors(all)
	if len(visitors) >= b.Max {
		var idle []flymachines.Machine
		for _, v := range visitors {
			if v.State != "started" && v.State != "starting" {
				idle = append(idle, v)
			}
		}
		if len(idle) == 0 {
			return flymachines.Machine{}, ErrFull
		}
		sort.Slice(idle, func(i, j int) bool { return b.lastUsed(idle[i]).Before(b.lastUsed(idle[j])) })
		if err := b.API.Delete(ctx, idle[0]); err != nil {
			return flymachines.Machine{}, err
		}
		b.forget(idle[0].Name)
	}
	env := map[string]string{
		"RC_TOKEN":       Token(b.Secret, name),
		"RC_PORTS_TOKEN": portsToken(b.Secret, name),
		"RC_PUBLIC_HOST": b.Host,
		"RC_PORT_URL":    "https://%d-" + strings.TrimPrefix(name, domain.VisitorPrefix) + "." + b.PortDomain + "/",
	}
	return b.API.Create(ctx, name, env, map[string]string{"rc-visitor": name})
}

// visitors filtra as máquinas de visitante (pg-...), ignorando as já destruídas.
func (b *FlyBackend) visitors(all []flymachines.Machine) []flymachines.Machine {
	var out []flymachines.Machine
	for _, m := range all {
		if strings.HasPrefix(m.Name, domain.VisitorPrefix) && m.State != "destroyed" && m.State != "destroying" {
			out = append(out, m)
		}
	}
	return out
}

// lastUsed é o último uso registrado; sem registro (o front reiniciou), a última mudança da máquina.
func (b *FlyBackend) lastUsed(m flymachines.Machine) time.Time {
	if t, ok := b.lastUse[m.Name]; ok {
		return t
	}
	return m.UpdatedAt
}

// remember guarda o IP privado e marca uso (com o lock tomado).
func (b *FlyBackend) remember(name, ip string) {
	if b.lastUse == nil {
		b.lastUse, b.ips = map[string]time.Time{}, map[string]string{}
	}
	b.lastUse[name] = b.Now()
	b.ips[name] = ip
}

// forget esquece a máquina apagada ou desligada (com o lock tomado): o terminal dela volta à entrada.
func (b *FlyBackend) forget(name string) {
	delete(b.ips, name)
}

// Touch marca uso agora.
func (b *FlyBackend) Touch(machine string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if _, ok := b.ips[machine]; ok {
		b.lastUse[machine] = b.Now()
	}
}

// ip é o endereço privado da máquina ligada; vazio se o front não a conhece ligada.
func (b *FlyBackend) ip(machine string) string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.ips[machine]
}

// Terminal é o proxy até o rc-visitor da máquina (terminal web dela), com o Host público intacto: é ele que o
// terminal confere. nil se a máquina não está ligada para este front.
func (b *FlyBackend) Terminal(machine string) http.Handler {
	ip := b.ip(machine)
	if ip == "" {
		return nil
	}
	return b.proxy(net.JoinHostPort(ip, strconv.Itoa(VisitorTerminalPort)), nil, func(w http.ResponseWriter, _ *http.Request, _ error) {
		http.Error(w, "the machine is starting, reload in a few seconds", http.StatusBadGateway)
	})
}

// Port leva a requisição à ponte de portas do rc-visitor, que disca a porta no loopback da máquina.
func (b *FlyBackend) Port(machine string, port uint16) http.Handler {
	ip := b.ip(machine)
	if ip == "" {
		return http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			http.Error(w, "this machine is not running; open the playground page to start it", http.StatusBadGateway)
		})
	}
	tok := portsToken(b.Secret, machine)
	return b.proxy(net.JoinHostPort(ip, strconv.Itoa(VisitorPortsPort)), func(h http.Header) {
		h.Set(HeaderPort, strconv.Itoa(int(port)))
		h.Set(HeaderToken, tok)
	}, portDown(port))
}

// proxy monta o reverse proxy até addr, repetindo a discagem enquanto o rc-visitor de uma máquina recém-ligada ainda
// não escuta.
func (b *FlyBackend) proxy(addr string, headers func(http.Header), onErr func(http.ResponseWriter, *http.Request, error)) http.Handler {
	return &httputil.ReverseProxy{
		Rewrite: func(pr *httputil.ProxyRequest) {
			pr.Out.URL.Scheme = "http"
			pr.Out.URL.Host = addr
			pr.Out.Host = pr.In.Host
			pr.Out.Header.Del(HeaderPort)
			pr.Out.Header.Del(HeaderToken)
			if headers != nil {
				headers(pr.Out.Header)
			}
			pr.SetXForwarded()
		},
		Transport:    &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) { return b.dialReady(ctx, addr) }},
		ErrorHandler: onErr,
	}
}

// dialReady disca addr de 250 em 250 ms até ReadyTimeout (o rc-visitor leva ~1 s para escutar depois do started).
func (b *FlyBackend) dialReady(ctx context.Context, addr string) (net.Conn, error) {
	deadline := time.Now().Add(b.ReadyTimeout)
	for {
		c, err := b.Dial(ctx, addr)
		if err == nil || time.Now().After(deadline) || ctx.Err() != nil {
			return c, err
		}
		time.Sleep(250 * time.Millisecond)
	}
}

// Reap desliga as máquinas sem uso há IdleStop e apaga as sem uso há MaxAge. Devolve o que fez, para o log.
func (b *FlyBackend) Reap(ctx context.Context) []string {
	b.mu.Lock()
	defer b.mu.Unlock()
	all, err := b.API.List(ctx)
	if err != nil {
		return []string{"list: " + err.Error()}
	}
	now := b.Now()
	var done []string
	for _, m := range b.visitors(all) {
		idle := now.Sub(b.lastUsed(m))
		switch {
		case idle > b.MaxAge:
			if err := b.API.Delete(ctx, m); err == nil {
				b.forget(m.Name)
				delete(b.lastUse, m.Name)
				done = append(done, "deleted "+m.Name)
			}
		case idle > b.IdleStop && m.State == "started":
			if err := b.API.Stop(ctx, m.ID); err == nil {
				b.forget(m.Name)
				done = append(done, "stopped "+m.Name)
			}
		}
	}
	return done
}

// portsToken é o token da ponte de portas, separado do token do terminal: a app do visitante não o vê no navegador.
func portsToken(secret []byte, machine string) string { return Token(secret, "ports:"+machine) }

// find acha a máquina pelo nome.
func find(all []flymachines.Machine, name string) (flymachines.Machine, bool) {
	for _, m := range all {
		if m.Name == name && m.State != "destroyed" && m.State != "destroying" {
			return m, true
		}
	}
	return flymachines.Machine{}, false
}

// remove tira a máquina da lista pelo id.
func remove(all []flymachines.Machine, id string) []flymachines.Machine {
	out := all[:0:0]
	for _, m := range all {
		if m.ID != id {
			out = append(out, m)
		}
	}
	return out
}
