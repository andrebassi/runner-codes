package androidfs

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Arquivos que os ajustes gravam no rootfs (todos com o prefixo rc-, para se distinguirem dos do Redroid).
const (
	tweakRC     = "system/etc/init/zz-rc-android.rc"
	tweakNet    = "system/bin/rc-android-net.sh"
	tweakRoutes = "system/bin/rc-android-routes.sh"
)

// netScript dá à eth0 da microVM o endereço do gateway do rc, ANTES do ipconfigstore do Redroid (o vendor roda no
// mesmo post-fs-data, depois deste arquivo, que mora no /system e é lido antes): ele lê a eth0 e grava a configuração
// ESTÁTICA que o EthernetTracker do Android aplica — sem DHCP (que o gateway do rc não tem), e com a rede passando a
// ser do Android: rota padrão, DNS e os apps vendo internet. O init da libkrun não configura a interface (o DHCP dele
// não acha servidor).
//
// Antes (até 06/11/2026) a eth0 era renomeada para vnet0, fora do alcance do Android: com ela chamada de eth0 e sem
// configuração estática, o NetworkStack morria tentando DHCP e o boot não fechava. Renomeada, o ipconfigstore não a
// achava, gravava a configuração sem endereço, e o Android ficava "sem rede" para os apps
// (net::ERR_INTERNET_DISCONNECTED com o ping funcionando como root — visto no The Designer).
const netScript = `#!/system/bin/sh
# Gravado pelo rc (internal/adapters/androidfs/tweak.go): a eth0 com o endereço do gateway do rc, para o ipconfigstore.
/system/bin/ip addr add %s dev eth0 2>/dev/null
/system/bin/ip link set eth0 up || exit 1
/system/bin/ip route add default via %s dev eth0 2>/dev/null
echo "rc-android: eth0 %s via %s" > /dev/kmsg
`

// routesScript devolve a tabela main ao roteamento. O netd troca as regras pelas dele (legacy_system, legacy_network,
// local_network e "32000: unreachable") e nenhuma consulta a main, onde está a rota da eth0: o SYN chega ao adbd e o
// SYN-ACK não tem rota de volta (medido em 06/11/2026). Roda no fim do boot, depois que o netd montou as dele.
const routesScript = `#!/system/bin/sh
# Gravado pelo rc (internal/adapters/androidfs/tweak.go): a tabela main antes do unreachable do netd.
/system/bin/ip rule del pref 31000 2>/dev/null
/system/bin/ip rule add pref 31000 from all lookup main
echo "rc-android: rota de volta pela tabela main" > /dev/kmsg
`

// initRC liga os dois scripts nos pontos certos do boot e fixa o ADB em TCP. Com /dev/vsock presente (a microVM tem),
// o adbd escuta SÓ em vsock:5555, e o gateway, que encaminha TCP, não chega nele (medido em 06/11/2026). O post-fs-
// data é o primeiro ponto em que o /system/bin/sh executa (o linker dele mora num APEX) e ainda antes do netd.
const initRC = `# Gravado pelo rc (internal/adapters/androidfs/tweak.go): o Redroid como PID 1 de uma microVM libkrun.
on early-init
    setprop service.adb.listen_addrs tcp:%d

on post-fs-data
    exec -- /` + tweakNet + `

on property:sys.boot_completed=1
    exec -- /` + tweakRoutes + `
`

// Tweak grava os ajustes no rootfs (idempotente: sobrescreve). n é o enlace do gateway do rc e a porta do adbd.
func Tweak(root string, n Net) error {
	if !n.Guest.IsValid() || !n.Gateway.IsValid() || n.ADBPort == 0 {
		return fmt.Errorf("android: tweak: guest, gateway and adb port are required")
	}
	files := []struct {
		rel  string
		mode os.FileMode
		body string
	}{
		{tweakNet, 0o755, fmt.Sprintf(netScript, n.Guest, n.Gateway, n.Guest, n.Gateway)},
		{tweakRoutes, 0o755, routesScript},
		{tweakRC, 0o644, fmt.Sprintf(initRC, n.ADBPort)},
	}
	for _, f := range files {
		path := filepath.Join(root, f.rel)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			return fmt.Errorf("android: tweak: %w", err)
		}
		if err := os.WriteFile(path, []byte(strings.TrimLeft(f.body, "\n")), f.mode); err != nil {
			return fmt.Errorf("android: tweak %s: %w", f.rel, err)
		}
		// WriteFile não troca o modo de um arquivo que já existia (start de uma máquina antiga).
		if err := os.Chmod(path, f.mode); err != nil {
			return fmt.Errorf("android: tweak %s: %w", f.rel, err)
		}
	}
	return nil
}
