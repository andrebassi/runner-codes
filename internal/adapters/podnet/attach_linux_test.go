//go:build linux

package podnet

import (
	"fmt"
	"net"
	"os"
	"runtime"
	"testing"

	"github.com/vishvananda/netlink"
	"github.com/vishvananda/netns"
)

// seq diferencia os netns dos testes do mesmo processo.
var seq int

// fakePodNetns monta o que um CNI deixaria: um netns com eth0 (ponta de um veth), IPv4 e rota
// padrão. Precisa de root; devolve o caminho do netns e apaga tudo no fim do teste.
func fakePodNetns(t *testing.T) string {
	t.Helper()
	if os.Geteuid() != 0 {
		t.Skip("precisa de root (netns, veth, tc)")
	}
	// Nome curto e único por teste: interface Linux tem no máximo 15 caracteres.
	seq++
	suffix := fmt.Sprintf("%d%d", os.Getpid()%10000, seq)
	name := "rcpod" + suffix
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()
	orig, _ := netns.Get()
	defer orig.Close()
	ns, err := netns.NewNamed(name)
	if err != nil {
		t.Fatal(err)
	}
	netns.Set(orig)
	t.Cleanup(func() { ns.Close(); netns.DeleteNamed(name) })
	veth := &netlink.Veth{LinkAttrs: netlink.LinkAttrs{Name: "rcvh" + suffix}, PeerName: "rcvp" + suffix}
	if err := netlink.LinkAdd(veth); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { netlink.LinkDel(veth) })
	peer, _ := netlink.LinkByName(veth.PeerName)
	if err := netlink.LinkSetNsFd(peer, int(ns)); err != nil {
		t.Fatal(err)
	}
	h, _ := netlink.NewHandleAt(ns)
	defer h.Close()
	p, _ := h.LinkByName(veth.PeerName)
	h.LinkSetName(p, PodIf)
	p, _ = h.LinkByName(PodIf)
	addr, _ := netlink.ParseAddr("10.99.0.2/24")
	h.AddrAdd(p, addr)
	h.LinkSetUp(p)
	h.RouteAdd(&netlink.Route{LinkIndex: p.Attrs().Index, Gw: net.ParseIP("10.99.0.1")})
	return "/run/netns/" + name
}

// TestAttach: lê o que o CNI configurou, cria o tap0 no netns do pod (e não no do host) e põe um
// filtro mirred em cada sentido.
func TestAttach(t *testing.T) {
	path := fakePodNetns(t)
	tap, info, err := Attach(path)
	if err != nil {
		t.Fatal(err)
	}
	defer tap.Close()
	if info.Addr.String() != "10.99.0.2/24" || info.Gateway.String() != "10.99.0.1" || len(info.MAC) != 6 {
		t.Fatalf("info %+v", info)
	}
	if _, err := netlink.LinkByName(TapIf); err == nil {
		t.Fatal("tap0 nasceu no netns do host")
	}
	ns, _ := netns.GetFromPath(path)
	defer ns.Close()
	h, _ := netlink.NewHandleAt(ns)
	defer h.Close()
	for _, ifname := range []string{PodIf, TapIf} {
		l, err := h.LinkByName(ifname)
		if err != nil {
			t.Fatalf("%s: %v", ifname, err)
		}
		fs, _ := h.FilterList(l, netlink.MakeHandle(0xffff, 0))
		if len(fs) != 1 {
			t.Fatalf("%s: %d filtros, esperava 1 mirred", ifname, len(fs))
		}
	}
}

// TestAttachErrors: netns inexistente e netns sem eth0 (ou sem IPv4) falham com o motivo.
func TestAttachErrors(t *testing.T) {
	if _, _, err := Attach("/run/netns/does-not-exist"); err == nil {
		t.Fatal("netns inexistente")
	}
	path := fakePodNetns(t)
	ns, _ := netns.GetFromPath(path)
	h, _ := netlink.NewHandleAt(ns)
	l, _ := h.LinkByName(PodIf)
	addrs, _ := h.AddrList(l, netlink.FAMILY_V4)
	for _, a := range addrs {
		h.AddrDel(l, &a)
	}
	if _, _, err := Attach(path); err == nil {
		t.Fatal("eth0 sem IPv4")
	}
	h.LinkSetName(l, "other0")
	if _, _, err := Attach(path); err == nil {
		t.Fatal("sem eth0")
	}
	h.Close()
	ns.Close()
}
