package k8sshim

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// specJSON serializa uma spec mínima com as anotações dadas.
func specJSON(t *testing.T, ann map[string]string, mod func(*specs.Spec)) []byte {
	t.Helper()
	s := specs.Spec{Process: &specs.Process{Args: []string{"sh"}}, Annotations: ann}
	if mod != nil {
		mod(&s)
	}
	b, err := json.Marshal(s)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

// TestParseBundleKinds: sandbox, container e ctr run (sem anotação) são classificados certo, e o
// netns vem da anotação do CRI ou, sem ela, do namespace de rede da spec.
func TestParseBundleKinds(t *testing.T) {
	sb, err := ParseBundle(specJSON(t, map[string]string{annContainerType: "sandbox", annSandboxID: "p1", annSandboxNetns: "/run/netns/a"}, nil))
	if err != nil || !sb.Sandbox || sb.SandboxID != "p1" || sb.Netns != "/run/netns/a" {
		t.Fatalf("sandbox %+v %v", sb, err)
	}
	c, _ := ParseBundle(specJSON(t, map[string]string{annContainerType: "container", annSandboxID: "p1"}, nil))
	if c.Sandbox {
		t.Fatal("container virou sandbox")
	}
	ctr, _ := ParseBundle(specJSON(t, nil, func(s *specs.Spec) {
		s.Linux = &specs.Linux{Namespaces: []specs.LinuxNamespace{{Type: specs.PIDNamespace}, {Type: specs.NetworkNamespace, Path: "/proc/9/ns/net"}}}
	}))
	if !ctr.Sandbox || ctr.SandboxID != "" || ctr.Netns != "/proc/9/ns/net" {
		t.Fatalf("ctr run %+v", ctr)
	}
}

// TestParseBundleErrors: JSON inválido e processo sem args são recusados.
func TestParseBundleErrors(t *testing.T) {
	if _, err := ParseBundle([]byte("{")); err == nil {
		t.Fatal("JSON inválido")
	}
	if _, err := ParseBundle([]byte(`{"process":{"args":[]}}`)); err == nil {
		t.Fatal("sem args")
	}
	if _, err := ReadBundle(t.TempDir()); err == nil {
		t.Fatal("sem config.json")
	}
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, "config.json"), specJSON(t, nil, nil), 0o644)
	if _, err := ReadBundle(dir); err != nil {
		t.Fatal(err)
	}
}

// TestGroupID: o grupo é o do pod (ou o rótulo próprio do rc), e o próprio id sem anotação.
func TestGroupID(t *testing.T) {
	if GroupID(&specs.Spec{Annotations: map[string]string{annSandboxID: "pod"}}, "c") != "pod" {
		t.Fatal("grupo do pod")
	}
	if GroupID(&specs.Spec{Annotations: map[string]string{"io.containerd.rc.v2.group": "g", annSandboxID: "pod"}}, "c") != "g" {
		t.Fatal("rótulo do rc vem primeiro")
	}
	if GroupID(&specs.Spec{}, "c") != "c" {
		t.Fatal("sem anotação, o id")
	}
}

// TestProcFromOCI: usuário, grupos, terminal e tamanho do console passam da spec.
func TestProcFromOCI(t *testing.T) {
	p := ProcFromOCI(&specs.Process{Args: []string{"a"}, Env: []string{"X=1"}, Cwd: "/w", Terminal: true,
		User: specs.User{UID: 5, GID: 6, AdditionalGids: []uint32{7}}, ConsoleSize: &specs.Box{Height: 30, Width: 100}})
	if p.UID != 5 || p.GID != 6 || p.Groups[0] != 7 || !p.Terminal || p.Rows != 30 || p.Cols != 100 || p.Cwd != "/w" {
		t.Fatalf("%+v", p)
	}
	if q := ProcFromOCI(&specs.Process{Args: []string{"a"}}); q.Rows != 0 {
		t.Fatal("sem console, sem tamanho")
	}
}

// TestPodResources: anotação válida muda o tamanho; inválida ou fora dos limites cai no padrão.
func TestPodResources(t *testing.T) {
	if r := PodResources(&specs.Spec{}); r.CPUs != defaultCPUs || r.MemoryMiB != defaultMemoryMiB {
		t.Fatalf("padrão %+v", r)
	}
	if r := PodResources(&specs.Spec{Annotations: map[string]string{annCPUs: "4", annMemory: "2048"}}); r.CPUs != 4 || r.MemoryMiB != 2048 {
		t.Fatalf("anotado %+v", r)
	}
	for _, bad := range []map[string]string{{annCPUs: "0", annMemory: "64"}, {annCPUs: "99", annMemory: "999999"}, {annCPUs: "x"}} {
		if r := PodResources(&specs.Spec{Annotations: bad}); r.CPUs != defaultCPUs || r.MemoryMiB != defaultMemoryMiB {
			t.Fatalf("%v aceito: %+v", bad, r)
		}
	}
}
