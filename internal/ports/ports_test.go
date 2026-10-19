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
		"no cpu":              mut(func(s *VMSpec) { s.CPUs = 0 }),
		"no memory":           mut(func(s *VMSpec) { s.MemoryMiB = 0 }),
		"two roots":           mut(func(s *VMSpec) { s.RootDir = "/x" }),
		"no root":             mut(func(s *VMSpec) { s.RootDisk = "" }),
		"no exec":             mut(func(s *VMSpec) { s.Exec = "" }),
		"socket without port": mut(func(s *VMSpec) { s.VsockPort = 0 }),
		"port without socket": mut(func(s *VMSpec) { s.VsockSocket = "" }),
		"argv with space":     mut(func(s *VMSpec) { s.Args = []string{"-c", "echo oi"} }),
		"empty argv":          mut(func(s *VMSpec) { s.Args = []string{""} }),
		"exec with newline":   mut(func(s *VMSpec) { s.Exec = "/a\nb" }),
		"tty with console":    mut(func(s *VMSpec) { s.TTY, s.Console = true, "/r/c.log" }),
	}
	for name, s := range bad {
		t.Run(name, func(t *testing.T) {
			if err := s.Validate(); err == nil {
				t.Fatal("expected rejection")
			}
		})
	}
}
