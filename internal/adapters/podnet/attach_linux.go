//go:build linux

package podnet

import (
	"errors"
	"fmt"
	"net/netip"
	"os"
	"runtime"

	"github.com/vishvananda/netlink"
	"github.com/vishvananda/netns"
	"golang.org/x/sys/unix"
)

// PodIf e TapIf são as interfaces do netns do pod: a eth0 que o CNI criou e o TAP que a VM usa.
const (
	PodIf = "eth0"
	TapIf = "tap0"
)

// mirredPrio é a prioridade dos filtros tc: alta o bastante (número grande = baixa prioridade no tc)
// para não brigar com filtro que um CNI (Cilium, Calico eBPF) tenha posto antes na mesma eth0.
const mirredPrio = 49152

// Attach prepara a rede do pod para a VM: lê a eth0 do netns nsPath, cria o tap0 lá dentro e liga
// os dois por tc mirred. Devolve o descritor do TAP (para Pump) e o que a eth0 tinha. Precisa de
// CAP_NET_ADMIN (o shim roda como root sob o containerd).
func Attach(nsPath string) (*os.File, Info, error) {
	ns, err := netns.GetFromPath(nsPath)
	if err != nil {
		return nil, Info{}, fmt.Errorf("pod netns %s: %w", nsPath, err)
	}
	defer ns.Close()
	h, err := netlink.NewHandleAt(ns)
	if err != nil {
		return nil, Info{}, fmt.Errorf("netlink in pod netns: %w", err)
	}
	defer h.Close()
	info, eth, err := inspect(h)
	if err != nil {
		return nil, Info{}, err
	}
	tap, err := createTap(ns, eth.Attrs().MTU)
	if err != nil {
		return nil, Info{}, err
	}
	tl, err := h.LinkByName(TapIf)
	if err == nil {
		err = h.LinkSetUp(tl)
	}
	if err == nil {
		err = redirect(h, eth.Attrs().Index, tl.Attrs().Index)
	}
	if err == nil {
		err = redirect(h, tl.Attrs().Index, eth.Attrs().Index)
	}
	if err != nil {
		tap.Close()
		return nil, Info{}, fmt.Errorf("tc mirred eth0<->tap0: %w", err)
	}
	return tap, info, nil
}

// inspect lê IPv4, prefixo, MAC, MTU e rota padrão da eth0 do pod.
func inspect(h *netlink.Handle) (Info, netlink.Link, error) {
	eth, err := h.LinkByName(PodIf)
	if err != nil {
		return Info{}, nil, fmt.Errorf("pod %s: %w", PodIf, err)
	}
	addrs, err := h.AddrList(eth, netlink.FAMILY_V4)
	if err != nil || len(addrs) == 0 {
		return Info{}, nil, fmt.Errorf("pod %s has no IPv4 address (err=%v)", PodIf, err)
	}
	ip, _ := netip.AddrFromSlice(addrs[0].IP.To4())
	ones, _ := addrs[0].Mask.Size()
	info := Info{Addr: netip.PrefixFrom(ip, ones), MAC: eth.Attrs().HardwareAddr, MTU: eth.Attrs().MTU}
	routes, _ := h.RouteList(eth, netlink.FAMILY_V4)
	for _, r := range routes {
		if r.Dst == nil || r.Dst.IP.IsUnspecified() {
			if gw, ok := netip.AddrFromSlice(r.Gw.To4()); ok {
				info.Gateway = gw
			}
		}
	}
	return info, eth, nil
}

// createTap abre o tap0 DENTRO do netns do pod: o /dev/net/tun cria a interface no netns da thread
// que o abre, então a thread entra no netns e volta. setns de rede é permitido em processo com
// várias threads (só o de mount exige thread única), desde que a thread fique presa à goroutine.
func createTap(ns netns.NsHandle, mtu int) (*os.File, error) {
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()
	orig, err := netns.Get()
	if err != nil {
		return nil, err
	}
	defer orig.Close()
	if err := netns.Set(ns); err != nil {
		return nil, fmt.Errorf("enter pod netns: %w", err)
	}
	defer netns.Set(orig)
	link := &netlink.Tuntap{
		LinkAttrs: netlink.LinkAttrs{Name: TapIf, MTU: mtu},
		Mode:      netlink.TUNTAP_MODE_TAP,
		// NO_PI: quadro Ethernet puro, sem o cabeçalho de 4 bytes do tun; ONE_QUEUE e NonPersist: o
		// TAP some quando o descritor fecha (a VM morreu), sem sobra no netns.
		Flags:      netlink.TUNTAP_NO_PI | netlink.TUNTAP_ONE_QUEUE,
		NonPersist: true,
		Queues:     1,
	}
	if err := netlink.LinkAdd(link); err != nil {
		return nil, fmt.Errorf("create %s: %w", TapIf, err)
	}
	if len(link.Fds) == 0 {
		return nil, errors.New("tap without descriptor")
	}
	return link.Fds[0], nil
}

// redirect põe na entrada de from um filtro que manda todo quadro para a saída de to.
func redirect(h *netlink.Handle, from, to int) error {
	q := &netlink.Ingress{QdiscAttrs: netlink.QdiscAttrs{LinkIndex: from, Handle: netlink.MakeHandle(0xffff, 0), Parent: netlink.HANDLE_INGRESS}}
	if err := h.QdiscAdd(q); err != nil && !errors.Is(err, unix.EEXIST) {
		return err
	}
	f := &netlink.MatchAll{
		FilterAttrs: netlink.FilterAttrs{LinkIndex: from, Parent: netlink.MakeHandle(0xffff, 0), Priority: mirredPrio, Protocol: unix.ETH_P_ALL},
		Actions:     []netlink.Action{netlink.NewMirredAction(to)},
	}
	return h.FilterAdd(f)
}
