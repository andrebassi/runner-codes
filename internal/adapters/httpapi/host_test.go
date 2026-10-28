package httpapi

import (
	"errors"
	"strings"
	"testing"

	"runner.codes/internal/app"
	"runner.codes/internal/domain"
)

// procMachines é o fakeMachines com três máquinas: uma ligada com processo, uma ligada cujo processo
// sumiu e uma desligada.
type procMachines struct{ *fakeMachines }

// List devolve as três máquinas.
func (procMachines) List() ([]app.MachineStatus, error) {
	return []app.MachineStatus{
		{Machine: domain.Machine{Name: "a"}, State: app.StateRunning, PID: 10},
		{Machine: domain.Machine{Name: "sumiu"}, State: app.StateRunning, PID: 11},
		{Machine: domain.Machine{Name: "off"}, State: app.StateStopped},
	}, nil
}

// TestHost: a rota soma ao que o host mediu quantas máquinas existem e quantas estão ligadas; erro da
// medição é 500, erro da lista vem como erro de máquina; sem Host a rota não existe.
func TestHost(t *testing.T) {
	measured := func() (domain.Host, error) { return domain.Host{DiskFreeMiB: 9000, KVM: true}, nil }
	s := &Server{Host: measured, Machines: &fakeMachines{files: map[string][]byte{}}}
	code, body := do(t, s, "GET", "/api/v1/host", "")
	if code != 200 || !strings.Contains(body, `"disk_free_mib":9000`) || !strings.Contains(body, `"kvm":true`) ||
		!strings.Contains(body, `"machines":1`) || !strings.Contains(body, `"running":1`) {
		t.Fatalf("host: %d %s", code, body)
	}
	// Sem serviço de máquinas: só a medição, contagens em zero.
	if code, body := do(t, &Server{Host: measured}, "GET", "/api/v1/host", ""); code != 200 || !strings.Contains(body, `"machines":0`) {
		t.Fatalf("sem máquinas: %d %s", code, body)
	}
	broken := &Server{Host: func() (domain.Host, error) { return domain.Host{}, errors.New("statfs falhou") }}
	if code, body := do(t, broken, "GET", "/api/v1/host", ""); code != 500 || !strings.Contains(body, "statfs falhou") {
		t.Fatalf("medição quebrada: %d %s", code, body)
	}
	listFails := &Server{Host: measured, Machines: &fakeMachines{err: errors.New("estado ilegível")}}
	if code, _ := do(t, listFails, "GET", "/api/v1/host", ""); code < 400 {
		t.Fatalf("lista quebrada: %d", code)
	}
	if code, _ := do(t, &Server{}, "GET", "/api/v1/host", ""); code != 404 {
		t.Fatalf("sem Host: %d", code)
	}
}

// TestHostProcs: cada máquina ligada leva o consumo do processo dela; processo que sumiu fica de fora
// (sem derrubar a rota) e a desligada não é medida.
func TestHostProcs(t *testing.T) {
	var asked []int
	s := &Server{
		Host:     func() (domain.Host, error) { return domain.Host{}, nil },
		Machines: procMachines{&fakeMachines{}},
		Proc: func(pid int) (int64, int64, error) {
			asked = append(asked, pid)
			if pid == 11 {
				return 0, 0, errors.New("sumiu")
			}
			return 5e9, 4096, nil
		},
	}
	code, body := do(t, s, "GET", "/api/v1/host", "")
	if code != 200 || !strings.Contains(body, `"procs":[{"name":"a","pid":10,"cpu_nanos":5000000000,"rss_bytes":4096}]`) ||
		!strings.Contains(body, `"running":2`) || !strings.Contains(body, `"machines":3`) {
		t.Fatalf("procs: %d %s", code, body)
	}
	if len(asked) != 2 {
		t.Fatalf("mediu %v (a desligada não devia)", asked)
	}
}
