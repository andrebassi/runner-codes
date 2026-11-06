package domain

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// Android faz da máquina um Android (Redroid) em vez de um Linux comum: o rootfs da imagem roda como diretório por
// virtio-fs, o /init do Android é o PID 1 (não há o agente do rc dentro), o kernel é a libkrunfw com binder, e a
// conversa com o convidado é pelo ADB, na porta publicada pelo gateway da máquina.
//
// A tela não é uma virtio-gpu (o Redroid desenha por software numa tela virtual): Width, Height e DPI vão como
// parâmetros de boot (androidboot.redroid_*), e quem vê a tela é o scrcpy pelo ADB.
type Android struct {
	Width  uint32 `json:"width"`
	Height uint32 `json:"height"`
	DPI    uint32 `json:"dpi"`
}

// DefaultAndroid é a tela de um celular comum (a medida na prova de 06/11/2026: 720x1280 a 320 dpi).
var DefaultAndroid = Android{Width: 720, Height: 1280, DPI: 320}

// ADBPort é a porta em que o adbd do convidado escuta (o rootfs fixa service.adb.listen_addrs=tcp:5555): a mesma
// do `adb tcpip` de um aparelho, que o scrcpy e o adb já assumem.
const ADBPort = 5555

// Limites da tela: abaixo de 320 px o launcher do Android não cabe; acima de 4096 a renderização por software não
// acompanha. O DPI vai do ldpi (120) ao xxxhdpi (640), as densidades que o Android conhece.
const (
	androidMinSide = 320
	androidMaxSide = 4096
	androidMinDPI  = 120
	androidMaxDPI  = 640
)

// ParseAndroid lê "LARGURAxALTURA[@DPI]" (ex. 720x1280@320); sem o @DPI, vale o do DefaultAndroid.
func ParseAndroid(s string) (Android, error) {
	size, dpiText, hasDPI := strings.Cut(strings.ToLower(strings.TrimSpace(s)), "@")
	w, h, ok := strings.Cut(size, "x")
	wn, err1 := strconv.ParseUint(w, 10, 32)
	hn, err2 := strconv.ParseUint(h, 10, 32)
	if !ok || err1 != nil || err2 != nil {
		return Android{}, fmt.Errorf("android %q must be WIDTHxHEIGHT[@DPI], e.g. 720x1280@320", s)
	}
	a := Android{Width: uint32(wn), Height: uint32(hn), DPI: DefaultAndroid.DPI}
	if hasDPI {
		dn, err := strconv.ParseUint(dpiText, 10, 32)
		if err != nil {
			return Android{}, fmt.Errorf("android %q: dpi %q is not a number", s, dpiText)
		}
		a.DPI = uint32(dn)
	}
	return a, a.Validate()
}

// Validate confere os limites da tela e da densidade.
func (a Android) Validate() error {
	for _, side := range []uint32{a.Width, a.Height} {
		if side < androidMinSide || side > androidMaxSide {
			return fmt.Errorf("android screen %dx%d must be between %dx%d and %dx%d",
				a.Width, a.Height, androidMinSide, androidMinSide, androidMaxSide, androidMaxSide)
		}
	}
	if a.DPI < androidMinDPI || a.DPI > androidMaxDPI {
		return fmt.Errorf("android dpi %d must be between %d and %d", a.DPI, androidMinDPI, androidMaxDPI)
	}
	return nil
}

// BootArgs são os parâmetros do /init do Redroid (o CMD da imagem mais a tela): qemu=1 e hardware=redroid escolhem
// o perfil de contêiner; gpu_mode=guest é a renderização por software (os workers não têm GPU).
func (a Android) BootArgs() []string {
	return []string{
		"qemu=1",
		"androidboot.hardware=redroid",
		"androidboot.redroid_gpu_mode=guest",
		fmt.Sprintf("androidboot.redroid_width=%d", a.Width),
		fmt.Sprintf("androidboot.redroid_height=%d", a.Height),
		fmt.Sprintf("androidboot.redroid_dpi=%d", a.DPI),
		// O DNS da rede que o ipconfigstore do Redroid grava para a eth0 (sem ele, a rede nasce sem servidor de nomes).
		"androidboot.redroid_net_ndns=2",
		"androidboot.redroid_net_dns1=8.8.8.8",
		"androidboot.redroid_net_dns2=1.1.1.1",
	}
}

// validateAndroid recusa o que uma máquina Android não tem como cumprir: tudo o que depende do agente do rc dentro
// do convidado (init, workload, usuário, sockets, docker, ssh-agent, volumes) e a tela por virtio-gpu, que o Android
// não usa. Sem imagem não há o rootfs do Redroid de onde nascer.
func validateAndroid(m Machine) error {
	if m.Android == nil {
		return nil
	}
	if err := m.Android.Validate(); err != nil {
		return err
	}
	switch {
	case m.Image == "":
		return errors.New("android needs image (a Redroid image, e.g. redroid/redroid:14.0.0_64only-latest)")
	case m.Display != nil || m.GPU:
		return errors.New("android draws its own screen (scrcpy over adb): drop display and gpu")
	case len(m.Init) > 0 || len(m.Workload) > 0 || m.User != "":
		return errors.New("android runs its own /init: init, workload and user are not available")
	case len(m.Mounts) > 0 || m.DockerSocket || m.SSHAgent || len(m.ExposeSockets) > 0 || len(m.MountSockets) > 0:
		return errors.New("android has no runner agent inside: volumes, sockets, docker_socket and ssh_agent are not available")
	}
	return nil
}
