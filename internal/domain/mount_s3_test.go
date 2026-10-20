package domain

import (
	"strings"
	"testing"
)

// TestS3Mount: s3://BUCKET[/PREFIX]:/GUEST[:ro] é lido sem confundir os dois-pontos do
// esquema; S3() separa bucket e prefixo (barras das pontas fora); diretório do host não é S3.
func TestS3Mount(t *testing.T) {
	for in, want := range map[string]Mount{
		"s3://bkt:/data":                 {Host: "s3://bkt", Guest: "/data"},
		"s3://bkt/datasets/2026:/d:ro":   {Host: "s3://bkt/datasets/2026", Guest: "/d", ReadOnly: true},
		"s3://my.bucket-1/x/:/srv/in:rw": {Host: "s3://my.bucket-1/x/", Guest: "/srv/in"},
	} {
		got, err := ParseMount(in)
		if err != nil || got != want {
			t.Fatalf("%q: %+v %v", in, got, err)
		}
	}
	for _, bad := range []string{"s3://:/d", "s3://bkt", "s3://bkt:/d:rx"} {
		if _, err := ParseMount(bad); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
	m, _ := ParseMount("s3://bkt/a/b/:/d")
	if b, p, ok := m.S3(); !ok || b != "bkt" || p != "a/b" {
		t.Fatalf("parts: %q %q %v", b, p, ok)
	}
	if b, p, ok := (Mount{Host: "s3://bkt"}).S3(); !ok || b != "bkt" || p != "" {
		t.Fatalf("no prefix: %q %q", b, p)
	}
	if _, _, ok := (Mount{Host: "/h"}).S3(); ok {
		t.Fatal("a host directory is not s3")
	}
}

// TestDiskMount: só um caminho absoluto do guest é volume de disco (sem lado no host); vai
// e volta pelo String; passa na validação sem rede.
func TestDiskMount(t *testing.T) {
	m, err := ParseMount("/var/lib/docker")
	if err != nil || !m.Disk() || m.Guest != "/var/lib/docker" || m.String() != "/var/lib/docker" {
		t.Fatalf("disk: %+v %v", m, err)
	}
	if _, err := ParseMount("var/lib/docker"); err == nil {
		t.Fatal("a relative single path is not a disk volume")
	}
	if (Mount{Host: "/h", Guest: "/g"}).Disk() {
		t.Fatal("a host directory is not a disk volume")
	}
	mc := Machine{Name: "d", Stack: "python", Mounts: []Mount{m}}.WithDefaults()
	if err := mc.Validate(); err != nil {
		t.Fatalf("a disk volume needs nothing else: %v", err)
	}
	mc.Mounts = []Mount{{Guest: "/proc/x"}}
	if err := mc.Validate(); err == nil {
		t.Fatal("reserved paths still apply")
	}
}

// TestS3MountRules: volume s3 exige rede (net ou política de saída), nome de bucket válido e
// destino absoluto; não precisa de caminho absoluto no "host" (não há host).
func TestS3MountRules(t *testing.T) {
	base := Machine{Name: "a", Stack: "python"}.WithDefaults()
	base.Mounts = []Mount{{Host: "s3://bkt/x", Guest: "/data", ReadOnly: true}}
	if err := base.Validate(); err == nil || !strings.Contains(err.Error(), "needs net") {
		t.Fatalf("an s3 volume without network: %v", err)
	}
	base.Network.Enabled = true
	if err := base.Validate(); err != nil {
		t.Fatal(err)
	}
	withPolicy := base
	withPolicy.Network = Network{Egress: Egress{AllowHosts: []string{"r2.cloudflarestorage.com"}}}
	if err := withPolicy.Validate(); err != nil {
		t.Fatalf("an egress policy implies network: %v", err)
	}
	for _, bad := range []Mount{
		{Host: "s3://Bkt", Guest: "/d"},
		{Host: "s3://ab", Guest: "/d"},
		{Host: "s3://-bkt", Guest: "/d"},
		{Host: "s3://bkt_x", Guest: "/d"},
		{Host: "s3://bkt", Guest: "data"},
		{Host: "s3://bkt", Guest: "/opt/runner"},
	} {
		m := base
		m.Mounts = []Mount{bad}
		if err := m.Validate(); err == nil {
			t.Fatalf("%+v must be refused", bad)
		}
	}
	withCred := base
	withCred.Network = Network{Credentials: []Credential{{Name: "a", Env: "A", Hosts: []string{"a.com"}}}}
	if err := withCred.Validate(); err != nil {
		t.Fatalf("a credential implies network: %v", err)
	}
	plain := base
	plain.Network = Network{}
	plain.Mounts = []Mount{{Host: "/h", Guest: "/d"}}
	if err := plain.Validate(); err != nil {
		t.Fatalf("a host directory needs no network: %v", err)
	}
}
