package netgw

import (
	"bytes"
	"context"
	"errors"
	"net/netip"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/header"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	gicmp "gvisor.dev/gvisor/pkg/tcpip/transport/icmp"
	"gvisor.dev/gvisor/pkg/waiter"

	"runner.codes/internal/domain"
)

// ping manda um echo do guest a dst pela pilha dele e diz se a resposta voltou em wait.
func (g *guestStack) ping(t *testing.T, dst string, wait time.Duration) bool {
	t.Helper()
	var wq waiter.Queue
	ep, err := g.s.NewEndpoint(gicmp.ProtocolNumber4, ipv4.ProtocolNumber, &wq)
	if err != nil {
		t.Fatal(err)
	}
	defer ep.Close()
	we, ch := waiter.NewChannelEntry(waiter.EventIn)
	wq.EventRegister(&we)
	defer wq.EventUnregister(&we)
	msg := make([]byte, header.ICMPv4MinimumSize+4)
	header.ICMPv4(msg).SetType(header.ICMPv4Echo)
	copy(msg[header.ICMPv4MinimumSize:], "rc!!")
	to := tcpip.FullAddress{NIC: 1, Addr: tcpip.AddrFrom4(netip.MustParseAddr(dst).As4())}
	if _, err := ep.Write(bytes.NewReader(msg), tcpip.WriteOptions{To: &to}); err != nil {
		t.Fatal(err)
	}
	deadline := time.After(wait)
	for {
		var out bytes.Buffer
		if _, err := ep.Read(&out, tcpip.ReadOptions{}); err == nil {
			return strings.HasSuffix(out.String(), "rc!!")
		}
		select {
		case <-ch:
		case <-deadline:
			return false
		}
	}
}

// pingRig é o rig do gateway com um Pinger falso que conta as chamadas e responde conforme ok.
func pingRig(t *testing.T, e domain.Egress, ok bool) (*rig, *atomic.Int32) {
	t.Helper()
	var calls atomic.Int32
	// O Pinger entra no Config antes do Start: trocá-lo com o gateway rodando é corrida.
	r := newRig(t, e, true, nil, func(c *Config) {
		c.Ping = func(_ context.Context, dst netip.Addr, payload []byte) error {
			calls.Add(1)
			if !ok || string(payload) != "rc!!" {
				return errors.New("sem resposta")
			}
			return nil
		}
	})
	return r, &calls
}

// TestICMPEcho: o ping do guest a um IP de fora só volta se o host recebeu a resposta dele;
// destino que a política nega nem chega ao host e fica no log de auditoria.
func TestICMPEcho(t *testing.T) {
	r, calls := pingRig(t, domain.Egress{}, true)
	if !r.guest.ping(t, "203.0.113.7", 3*time.Second) || calls.Load() != 1 {
		t.Fatalf("allowed ping: calls %d", calls.Load())
	}
	// Os prazos são largos: com -race o echo leva mais de 500 ms até o gateway (ARP + lentidão).
	r, calls = pingRig(t, domain.Egress{}, false)
	if r.guest.ping(t, "203.0.113.7", 2*time.Second) {
		t.Fatal("a ping the host got no answer for must not be answered to the guest")
	}
	if calls.Load() != 1 {
		t.Fatalf("the host must have been asked once: %d", calls.Load())
	}
	r, calls = pingRig(t, domain.Egress{AllowCIDRs: []string{"198.51.100.0/24"}}, true)
	if r.guest.ping(t, "203.0.113.7", 2*time.Second) {
		t.Fatal("a denied ping must not be answered")
	}
	if calls.Load() != 0 {
		t.Fatalf("a denied ping must not reach the host: %d", calls.Load())
	}
	if !strings.Contains(r.log.String(), "deny icmp 203.0.113.7") {
		t.Fatalf("audit log: %q", r.log.String())
	}
	if !r.guest.ping(t, "198.51.100.9", 3*time.Second) {
		t.Fatal("the allowed CIDR answers")
	}
}

// TestICMPInFlightCap: com o semáforo cheio o echo some (não fila nem trava a pilha).
func TestICMPInFlightCap(t *testing.T) {
	r, calls := pingRig(t, domain.Egress{}, true)
	for i := 0; i < maxPingsInFlight; i++ {
		r.gw.pings <- struct{}{}
	}
	if r.guest.ping(t, "203.0.113.7", 300*time.Millisecond) || calls.Load() != 0 {
		t.Fatal("over the cap the echo is dropped")
	}
}

// TestHostPing: o Pinger padrão alcança o 127.0.0.1 do host sem root, e um destino mudo
// (TEST-NET, que ninguém roteia) termina no prazo com erro.
func TestHostPing(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	if err := hostPing(ctx, netip.MustParseAddr("127.0.0.1"), []byte("eco")); err != nil {
		t.Skipf("unprivileged ICMP unavailable here: %v", err)
	}
	ctx2, cancel2 := context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer cancel2()
	if err := hostPing(ctx2, netip.MustParseAddr("192.0.2.1"), []byte("eco")); err == nil {
		t.Fatal("TEST-NET must not answer")
	}
}

// TestIsEchoReply: só a resposta com a mesma sequência e os mesmos dados conta.
func TestIsEchoReply(t *testing.T) {
	reply := func(typ header.ICMPv4Type, seq uint16, data string) []byte {
		b := make([]byte, header.ICMPv4MinimumSize+len(data))
		h := header.ICMPv4(b)
		h.SetType(typ)
		h.SetSequence(seq)
		copy(b[header.ICMPv4MinimumSize:], data)
		return b
	}
	cases := map[string]bool{
		"ok":         isEchoReply(reply(header.ICMPv4EchoReply, 1, "x"), 1, []byte("x")),
		"other seq":  !isEchoReply(reply(header.ICMPv4EchoReply, 2, "x"), 1, []byte("x")),
		"other data": !isEchoReply(reply(header.ICMPv4EchoReply, 1, "y"), 1, []byte("x")),
		"unreach":    !isEchoReply(reply(header.ICMPv4DstUnreachable, 1, "x"), 1, []byte("x")),
		"garbage":    !isEchoReply([]byte{1}, 1, nil),
	}
	for name, ok := range cases {
		if !ok {
			t.Errorf("%s", name)
		}
	}
}
