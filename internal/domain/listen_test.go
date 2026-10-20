package domain

import (
	"reflect"
	"testing"
)

// procSample imita /proc/net/tcp + /proc/net/tcp6 de uma máquina com: uvicorn em 0.0.0.0:8000, um dev server em
// 127.0.0.1:5173, um serviço só na eth0 (100.96.0.2:9000), uma conexão estabelecida (não LISTEN) e, no IPv6, um
// servidor em [::]:3000, um em [::1]:4000, um v4 mapeado ::ffff:127.0.0.1:4500 e o mesmo 8000 repetido.
const procSample = `  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
   0: 00000000:1F40 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 1 1 0 100 0 0 10 0
   1: 0100007F:1435 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 2 1 0 100 0 0 10 0
   2: 02006064:2328 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 3 1 0 100 0 0 10 0
   3: 02006064:8F1A 112EBE23:01BB 01 00000000:00000000 02:00000016 00000000     0        0 4 2 0 21 4 30 10 -1
  sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
   0: 00000000000000000000000000000000:0BB8 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 5 1 0 100 0 0 10 0
   1: 00000000000000000000000001000000:0FA0 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 6 1 0 100 0 0 10 0
   2: 0000000000000000FFFF00000100007F:1194 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 7 1 0 100 0 0 10 0
   3: 00000000000000000000000000000000:1F40 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 8 1 0 100 0 0 10 0
`

// TestParseListeningPorts: só LISTEN alcançável pelo loopback do guest, em ordem e sem repetir.
func TestParseListeningPorts(t *testing.T) {
	got := ParseListeningPorts(procSample)
	want := []uint16{3000, 4000, 4500, 5173, 8000}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("portas %v, queria %v", got, want)
	}
}

// TestParseListeningPortsJunk: entrada vazia, linha quebrada, porta 0, porta fora de hexa e endereço de tamanho
// estranho não viram porta nem derrubam o parser.
func TestParseListeningPortsJunk(t *testing.T) {
	junk := "\n" +
		"   0: 00000000:0000 00000000:0000 0A x\n" + // porta 0
		"   1: 00000000:ZZZZ 00000000:0000 0A x\n" + // porta não hexa
		"   2: 00000000 00000000:0000 0A x\n" + // sem ':' no endereço
		"   3: 0000:1F40 00000000:0000 0A x\n" + // endereço de tamanho estranho
		"   4: 0000000000000000FFFF000002006064:1F40 0:0 0A x\n" + // v4 mapeado, mas o IP da eth0
		"   5: 00000000000000000000000002000000:1F40 0:0 0A x\n" + // IPv6 qualquer, nem curinga nem ::1
		"   6: 00000000:1F40\n" // linha curta
	if got := ParseListeningPorts(junk); len(got) != 0 {
		t.Fatalf("lixo virou porta: %v", got)
	}
	if got := ParseListeningPorts(""); len(got) != 0 {
		t.Fatalf("vazio: %v", got)
	}
}
