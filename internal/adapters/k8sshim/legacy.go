package k8sshim

import (
	"encoding/json"

	bootapi "github.com/containerd/containerd/api/runtime/bootstrap/v1"
	"github.com/containerd/containerd/v2/pkg/protobuf/proto"
)

// IsLegacyStart diz se o "start" veio de um containerd anterior ao 2.3 (o do microk8s 1.35 é o 2.1.6):
// o novo manda BootstrapParams em protobuf no stdin com o id e o namespace da linha de comando; o antigo
// manda vazio ou as opções do runtime. O critério é o mesmo do pkg/shim (compat.go).
func IsLegacyStart(stdin []byte, id, namespace string) bool {
	if len(stdin) == 0 {
		return true
	}
	var p bootapi.BootstrapParams
	if proto.Unmarshal(stdin, &p) != nil {
		return true
	}
	return p.InstanceID != id || p.Namespace != namespace
}

// legacyResult é a resposta do start no formato que o containerd 2.0–2.2 lê do stdout do shim.
type legacyResult struct {
	Version  int32  `json:"version"`
	Address  string `json:"address"`
	Protocol string `json:"protocol"`
}

// FormatStartResult traduz a resposta do start: o pkg/shim 2.3 sempre escreve BootstrapResult em
// protobuf, e o containerd antigo lia esses bytes como endereço ("unsupported protocol: Yunix", medido no
// microk8s). Para chamada antiga sai o JSON de antes; para a nova, os bytes como vieram.
func FormatStartResult(out []byte, legacy bool) []byte {
	if !legacy {
		return out
	}
	var r bootapi.BootstrapResult
	if proto.Unmarshal(out, &r) != nil || r.Address == "" {
		return out
	}
	b, _ := json.Marshal(legacyResult{Version: r.Version, Address: r.Address, Protocol: r.Protocol})
	return b
}
