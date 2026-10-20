package k8sshim

import (
	"fmt"
	"path"
	"slices"
	"strings"

	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// GuestShare é onde o guest monta o virtio-fs do pod; cada container fica em <GuestShare>/<id>/.
const GuestShare = "/run/rc-pod"

// ShareTag é a tag do virtio-fs do pod.
const ShareTag = "rcpod"

// HostCopy é uma montagem bind da spec cuja origem no host foi copiada para o share (volume,
// /etc/hosts, resolv.conf, serviceaccount): Index nomeia o diretório em <share>/mounts/.
type HostCopy struct {
	Source string
	Index  int
}

// devNodes são os dispositivos que todo container espera em /dev: o /dev do container é um tmpfs
// novo, e cada um vem do /dev da VM por bind (criar com mknod pediria o número de cada um).
var devNodes = []string{"null", "zero", "full", "random", "urandom", "tty"}

// shellQuote põe o valor entre aspas simples para o sh do guest: nada do caminho vira comando.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// cleanDest junta o destino ao root do container sem deixar ".." escapar dele.
func cleanDest(root, dest string) string {
	return path.Join(root, path.Clean("/"+dest))
}

// mountOptions separa as opções que o mount do busybox entende das flags de bind (ro/rbind), que a
// spec mistura no mesmo campo.
func mountOptions(opts []string) (readOnly bool, rest []string) {
	for _, o := range opts {
		switch o {
		case "ro":
			readOnly = true
		case "rw", "bind", "rbind", "rprivate", "private", "rslave", "slave", "rshared", "shared":
			// Propagação não atravessa a fronteira da VM (limite declarado); bind é o próprio tipo.
		default:
			rest = append(rest, o)
		}
	}
	return readOnly, rest
}

// MountScript gera o script sh que o guest roda (OpExec) para montar o container cujo rootfs está em
// root: as montagens de sistema da spec (proc, sysfs, tmpfs, devpts, mqueue), os nós de /dev, e as
// montagens bind já copiadas para o share. Função pura: quem copia é o host, quem executa é o guest.
func MountScript(root string, mounts []specs.Mount, copies map[string]HostCopy, share string) string {
	var b strings.Builder
	b.WriteString("set -e\n")
	q := shellQuote
	devTmpfs := false
	for _, m := range mounts {
		dst := cleanDest(root, m.Destination)
		ro, opts := mountOptions(m.Options)
		optArg := ""
		if len(opts) > 0 {
			optArg = " -o " + q(strings.Join(opts, ","))
		}
		switch m.Type {
		case "proc", "sysfs", "tmpfs", "devpts", "mqueue":
			fmt.Fprintf(&b, "mkdir -p %s\n", q(dst))
			if m.Type == "devpts" {
				// newinstance: um /dev/pts só do container; ptmxmode para o /dev/ptmx abrir sem root.
				optArg = " -o " + q("newinstance,ptmxmode=0666,mode=620,gid=5")
			}
			fmt.Fprintf(&b, "mount -t %s %s %s%s\n", m.Type, m.Type, q(dst), optArg)
			if m.Destination == "/dev" && m.Type == "tmpfs" {
				devTmpfs = true
			}
		case "cgroup", "cgroup2":
			// Sem cgroup aninhado na v1: os limites do pod valem para a VM inteira.
		default:
			c, ok := copies[m.Source]
			if !ok {
				continue
			}
			src := path.Join(share, "mounts", fmt.Sprint(c.Index))
			// A cópia pode ser arquivo (hosts, resolv.conf) ou diretório (volume): o destino nasce do
			// mesmo tipo, senão o bind falha com "not a directory".
			fmt.Fprintf(&b, "if [ -d %s ]; then mkdir -p %s; else mkdir -p %s; touch %s; fi\n", q(src), q(dst), q(path.Dir(dst)), q(dst))
			fmt.Fprintf(&b, "mount --bind %s %s\n", q(src), q(dst))
			if ro {
				fmt.Fprintf(&b, "mount -o remount,bind,ro %s\n", q(dst))
			}
		}
	}
	if devTmpfs {
		for _, d := range devNodes {
			dst := cleanDest(root, "/dev/"+d)
			fmt.Fprintf(&b, "touch %s && mount --bind /dev/%s %s\n", q(dst), d, q(dst))
		}
		fmt.Fprintf(&b, "ln -sf pts/ptmx %s\n", q(cleanDest(root, "/dev/ptmx")))
		fmt.Fprintf(&b, "ln -sf /proc/self/fd %s\n", q(cleanDest(root, "/dev/fd")))
	}
	return b.String()
}

// UnmountScript desmonta tudo o que está sob root, do mais fundo para o mais raso: o host só apaga a
// cópia do container depois (montagem viva no guest deixaria o rm pela metade). O root é sempre um
// caminho gerado pelo shim (GuestShare/<id>/rootfs), sem espaço nem caractere de regex.
func UnmountScript(root string) string {
	return fmt.Sprintf("grep -o ' %s[^ ]*' /proc/mounts | sort -r | while read m; do umount -l \"$m\"; done; true\n", strings.TrimSuffix(root, "/"))
}

// CopiesFor escolhe, das montagens da spec, as que vêm de arquivo ou diretório do host (bind) e dá a
// cada origem um índice. A mesma origem em dois containers do pod (um emptyDir) pode receber o índice
// que já tinha: assim os dois veem a mesma cópia.
func CopiesFor(mounts []specs.Mount, known map[string]HostCopy) map[string]HostCopy {
	out := map[string]HostCopy{}
	next := len(known)
	for _, m := range mounts {
		if m.Type != "bind" && !slices.Contains(m.Options, "bind") && !slices.Contains(m.Options, "rbind") {
			continue
		}
		if c, ok := known[m.Source]; ok {
			out[m.Source] = c
			continue
		}
		if _, ok := out[m.Source]; ok {
			continue
		}
		out[m.Source] = HostCopy{Source: m.Source, Index: next}
		next++
	}
	return out
}
