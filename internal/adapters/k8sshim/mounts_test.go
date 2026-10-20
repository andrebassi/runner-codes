package k8sshim

import (
	"strings"
	"testing"

	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// TestMountScript confere o script de um container típico do CRI: sistema de arquivos de sistema,
// /dev com os nós e o ptmx, bind somente leitura e de arquivo, cgroup ignorado e bind sem cópia pulado.
func TestMountScript(t *testing.T) {
	root := "/run/rc-pod/c1/rootfs"
	mounts := []specs.Mount{
		{Destination: "/proc", Type: "proc", Source: "proc"},
		{Destination: "/dev", Type: "tmpfs", Source: "tmpfs", Options: []string{"nosuid", "mode=755"}},
		{Destination: "/dev/pts", Type: "devpts", Source: "devpts", Options: []string{"gid=5"}},
		{Destination: "/sys/fs/cgroup", Type: "cgroup", Source: "cgroup"},
		{Destination: "/etc/hosts", Type: "bind", Source: "/kubelet/hosts", Options: []string{"rbind", "rprivate", "rw"}},
		{Destination: "/var/run/secrets/sa", Type: "bind", Source: "/kubelet/sa", Options: []string{"rbind", "ro"}},
		{Destination: "/not/copied", Type: "bind", Source: "/elsewhere"},
	}
	copies := map[string]HostCopy{"/kubelet/hosts": {Index: 0}, "/kubelet/sa": {Index: 1}}
	s := MountScript(root, mounts, copies, GuestShare)
	for _, want := range []string{
		"set -e\n",
		"mount -t proc proc '/run/rc-pod/c1/rootfs/proc'\n",
		"mount -t tmpfs tmpfs '/run/rc-pod/c1/rootfs/dev' -o 'nosuid,mode=755'\n",
		"mount -t devpts devpts '/run/rc-pod/c1/rootfs/dev/pts' -o 'newinstance,ptmxmode=0666,mode=620,gid=5'\n",
		"mount --bind '/run/rc-pod/mounts/0' '/run/rc-pod/c1/rootfs/etc/hosts'\n",
		"mount -o remount,bind,ro '/run/rc-pod/c1/rootfs/var/run/secrets/sa'\n",
		"mount --bind /dev/null '/run/rc-pod/c1/rootfs/dev/null'\n",
		"ln -sf pts/ptmx '/run/rc-pod/c1/rootfs/dev/ptmx'\n",
	} {
		if !strings.Contains(s, want) {
			t.Fatalf("faltou %q em:\n%s", want, s)
		}
	}
	if strings.Contains(s, "cgroup") || strings.Contains(s, "/not/copied") {
		t.Fatalf("cgroup ou bind sem cópia entraram:\n%s", s)
	}
	if strings.Count(s, "remount,bind,ro") != 1 {
		t.Fatal("só o bind ro é remontado")
	}
}

// TestMountScriptNoDevTmpfs: sem tmpfs em /dev, nenhum nó é criado (o /dev da spec é outro).
func TestMountScriptNoDevTmpfs(t *testing.T) {
	s := MountScript("/r", []specs.Mount{{Destination: "/tmp", Type: "tmpfs", Source: "tmpfs"}}, nil, GuestShare)
	if strings.Contains(s, "/dev/null") {
		t.Fatal(s)
	}
}

// TestShellQuoteAndClean: aspas não fecham o argumento, e ".." no destino não sai do root.
func TestShellQuoteAndClean(t *testing.T) {
	if shellQuote(`a'b`) != `'a'\''b'` {
		t.Fatal(shellQuote(`a'b`))
	}
	if cleanDest("/r", "../../etc/passwd") != "/r/etc/passwd" {
		t.Fatal(cleanDest("/r", "../../etc/passwd"))
	}
}

// TestUnmountScript: desmonta o que está sob o root e nunca falha (o rm é do host).
func TestUnmountScript(t *testing.T) {
	s := UnmountScript("/run/rc-pod/c1/rootfs/")
	if !strings.Contains(s, "' /run/rc-pod/c1/rootfs[^ ]*'") || !strings.HasSuffix(s, "true\n") {
		t.Fatal(s)
	}
}

// TestCopiesFor: só bind entra; origem repetida (no mesmo container ou já conhecida do pod) reaproveita
// o índice, e as novas continuam a numeração.
func TestCopiesFor(t *testing.T) {
	known := map[string]HostCopy{"/shared": {Source: "/shared", Index: 0}}
	got := CopiesFor([]specs.Mount{
		{Type: "proc", Source: "proc"},
		{Type: "bind", Source: "/shared"},
		{Type: "none", Source: "/a", Options: []string{"rbind"}},
		{Type: "none", Source: "/a", Options: []string{"bind"}},
		{Type: "bind", Source: "/b"},
	}, known)
	if len(got) != 3 || got["/shared"].Index != 0 || got["/a"].Index != 1 || got["/b"].Index != 2 {
		t.Fatalf("%+v", got)
	}
}
