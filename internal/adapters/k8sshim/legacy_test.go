package k8sshim

import (
	"encoding/json"
	"testing"

	bootapi "github.com/containerd/containerd/api/runtime/bootstrap/v1"
	"github.com/containerd/containerd/v2/pkg/protobuf/proto"
)

// TestIsLegacyStart: vazio, lixo (opções do runtime) e parâmetros de outro id são chamada antiga; os
// parâmetros novos com o mesmo id e namespace, não.
func TestIsLegacyStart(t *testing.T) {
	good, _ := proto.Marshal(&bootapi.BootstrapParams{InstanceID: "c1", Namespace: "k8s.io"})
	if IsLegacyStart(good, "c1", "k8s.io") {
		t.Fatal("chamada nova vista como antiga")
	}
	for _, in := range [][]byte{nil, {0xff, 0xff}, good} {
		id := "c1"
		if len(in) > 2 {
			id = "outro"
		}
		if !IsLegacyStart(in, id, "k8s.io") {
			t.Fatalf("%v não visto como antigo", in)
		}
	}
}

// TestFormatStartResult: chamada antiga recebe o JSON com versão, endereço e protocolo; a nova recebe
// os bytes intactos; resposta que não decodifica passa como veio.
func TestFormatStartResult(t *testing.T) {
	raw, _ := proto.Marshal(&bootapi.BootstrapResult{Version: 3, Address: "unix:///run/s/abc", Protocol: "ttrpc"})
	var got legacyResult
	if err := json.Unmarshal(FormatStartResult(raw, true), &got); err != nil || got.Address != "unix:///run/s/abc" || got.Version != 3 || got.Protocol != "ttrpc" {
		t.Fatalf("%+v %v", got, err)
	}
	if string(FormatStartResult(raw, false)) != string(raw) {
		t.Fatal("chamada nova alterada")
	}
	if string(FormatStartResult([]byte{0xff}, true)) != "\xff" {
		t.Fatal("lixo alterado")
	}
}
