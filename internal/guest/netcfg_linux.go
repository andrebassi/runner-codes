//go:build linux

package guest

import (
	"fmt"
	"net"
	"time"

	"github.com/vishvananda/netlink"
)

// linkWait é quanto o guest espera a eth0 aparecer: o driver virtio-net registra a placa
// logo no boot, mas o init pode chegar antes dele; 2 s cobre com sobra sem atrasar a falha.
const linkWait = 2 * time.Second

// configureLinkOS sobe lo e eth0, põe o endereço e a rota padrão via gateway, por netlink
// (sem depender de `ip` na imagem: o fedora:42 nem traz o iproute).
func configureLinkOS(cfg NetConfig) error {
	if lo, err := netlink.LinkByName("lo"); err == nil {
		netlink.LinkSetUp(lo)
	}
	var link netlink.Link
	var err error
	for end := time.Now().Add(linkWait); ; time.Sleep(20 * time.Millisecond) {
		if link, err = netlink.LinkByName("eth0"); err == nil || time.Now().After(end) {
			break
		}
	}
	if err != nil {
		return fmt.Errorf("eth0 not found: %w", err)
	}
	ipnet := &net.IPNet{IP: cfg.Addr.Addr().AsSlice(), Mask: net.CIDRMask(cfg.Addr.Bits(), cfg.Addr.Addr().BitLen())}
	if err := netlink.AddrReplace(link, &netlink.Addr{IPNet: ipnet}); err != nil {
		return fmt.Errorf("address: %w", err)
	}
	if err := netlink.LinkSetUp(link); err != nil {
		return fmt.Errorf("up: %w", err)
	}
	if cfg.GatewayOnLink() {
		gw := &net.IPNet{IP: cfg.Gateway.AsSlice(), Mask: net.CIDRMask(cfg.Gateway.BitLen(), cfg.Gateway.BitLen())}
		if err := netlink.RouteReplace(&netlink.Route{LinkIndex: link.Attrs().Index, Dst: gw, Scope: netlink.SCOPE_LINK}); err != nil {
			return fmt.Errorf("gateway link route: %w", err)
		}
	}
	route := &netlink.Route{LinkIndex: link.Attrs().Index, Gw: cfg.Gateway.AsSlice()}
	if err := netlink.RouteReplace(route); err != nil {
		return fmt.Errorf("default route: %w", err)
	}
	return nil
}
