package ports

import "testing"

// valid é uma spec de VM de job aceitável.
func valid() VMSpec {
	return VMSpec{CPUs: 1, MemoryMiB: 512, RootDisk: "/img/a.ext4", VsockSocket: "/r/v.sock", VsockPort: 5000, Exec: "/opt/runner/start.sh"}
}

// TestValidate cobre cada recusa, inclusive o argv com espaço que a libkrun parte em palavras.
func TestValidate(t *testing.T) {
	if err := valid().Validate(); err != nil {
		t.Fatal(err)
	}
	mut := func(f func(*VMSpec)) VMSpec { s := valid(); f(&s); return s }
	bad := map[string]VMSpec{
		"sem cpu":          mut(func(s *VMSpec) { s.CPUs = 0 }),
		"sem memória":      mut(func(s *VMSpec) { s.MemoryMiB = 0 }),
		"dois roots":       mut(func(s *VMSpec) { s.RootDir = "/x" }),
		"nenhum root":      mut(func(s *VMSpec) { s.RootDisk = "" }),
		"sem exec":         mut(func(s *VMSpec) { s.Exec = "" }),
		"socket sem porta": mut(func(s *VMSpec) { s.VsockPort = 0 }),
		"porta sem socket": mut(func(s *VMSpec) { s.VsockSocket = "" }),
		"argv com espaço":  mut(func(s *VMSpec) { s.Args = []string{"-c", "echo oi"} }),
		"argv vazio":       mut(func(s *VMSpec) { s.Args = []string{""} }),
		"exec com newline": mut(func(s *VMSpec) { s.Exec = "/a\nb" }),
	}
	for name, s := range bad {
		t.Run(name, func(t *testing.T) {
			if err := s.Validate(); err == nil {
				t.Fatal("esperava recusa")
			}
		})
	}
}
