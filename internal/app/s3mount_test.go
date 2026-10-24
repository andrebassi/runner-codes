package app

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// TestDiskVolumesAndDockerSocket: volume de disco vira bind de /workspace/.disk/<caminho> no
// start (como root), sem device virtio-fs; docker_socket põe a ponte na spec, do docker.sock
// no diretório da máquina para o /var/run/docker.sock do guest.
func TestDiskVolumesAndDockerSocket(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "dk", Stack: "python", DockerSocket: true, User: "app",
		Mounts: []domain.Mount{{Guest: "/var/lib/docker"}}})
	if _, err := m.Start(ctx, "dk"); err != nil {
		t.Fatal(err)
	}
	spec := d.specs[len(d.specs)-1]
	if len(spec.Mounts) != 0 {
		t.Fatalf("a disk volume is not a virtio-fs device: %+v", spec.Mounts)
	}
	if len(spec.Sockets) != 1 || spec.Sockets[0].Host != filepath.Join(st.Dir("dk"), MachineDockerSocket) || spec.Sockets[0].Guest != "/var/run/docker.sock" {
		t.Fatalf("docker socket bridge: %+v", spec.Sockets)
	}
	var bind *protocol.Job
	for i, j := range g.jobs {
		if strings.HasPrefix(j.TraceID, "mount-disk-") {
			bind = &g.jobs[i]
		}
	}
	if bind == nil || bind.Argv[4] != "/workspace/.disk/var/lib/docker" || bind.Argv[5] != "/var/lib/docker" || bind.User != "" {
		t.Fatalf("disk bind job (as root): %+v", bind)
	}
}

// TestS3Mounts: volume s3 não vira device virtio-fs na spec nem passa pela checagem de
// diretório do host; no start, o guest recebe mount-s3 com bucket, prefixo, :ro e o env da
// máquina (as credenciais), na ordem das montagens; bucket recusado falha o start com o motivo.
func TestS3Mounts(t *testing.T) {
	m, _, _, d, g := machinesFixture(t)
	ctx := context.Background()
	dir := t.TempDir()
	mc := domain.Machine{Name: "s3", Stack: "python", Network: domain.Network{Enabled: true},
		Env:    []string{"AWS_ACCESS_KEY_ID=k", "AWS_SECRET_ACCESS_KEY=s"},
		Mounts: []domain.Mount{{Host: dir, Guest: "/local"}, {Host: "s3://bkt/data/", Guest: "/data", ReadOnly: true}}}
	if _, err := m.Create(ctx, mc); err != nil {
		t.Fatalf("create with an s3 volume: %v", err)
	}
	if _, err := m.Start(ctx, "s3"); err != nil {
		t.Fatal(err)
	}
	spec := d.specs[len(d.specs)-1]
	if len(spec.Mounts) != 1 || spec.Mounts[0].Tag != "vol0" {
		t.Fatalf("only the host directory is a virtio-fs device: %+v", spec.Mounts)
	}
	var s3 []protocol.Job
	for _, j := range g.jobs {
		if j.Op == protocol.OpMountS3 {
			s3 = append(s3, j)
		}
	}
	if len(s3) != 1 {
		t.Fatalf("one mount-s3 per s3 volume: %+v", s3)
	}
	j := s3[0]
	if j.Path != "/data" || j.S3.Bucket != "bkt" || j.S3.Prefix != "data" || !j.S3.ReadOnly || strings.Join(j.Env, " ") != "AWS_ACCESS_KEY_ID=k AWS_SECRET_ACCESS_KEY=s" {
		t.Fatalf("mount-s3 job: %+v %+v", j, j.S3)
	}
	m.Create(ctx, domain.Machine{Name: "deny", Stack: "python", Network: domain.Network{Enabled: true},
		Mounts: []domain.Mount{{Host: "s3://denied", Guest: "/d"}}})
	if _, err := m.Start(ctx, "deny"); err == nil || !strings.Contains(err.Error(), "AccessDenied") {
		t.Fatalf("a refused bucket fails the start with the reason: %v", err)
	}
}
