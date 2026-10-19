package app

import (
	"context"
	"errors"
	"testing"
	"time"

	"runner.code/internal/domain"
)

// gpuFixtures é o pool falso com a stack llama declarando GPU e VRAM no catálogo.
func gpuFixtures(t *testing.T) (*Executor, *Pool, *launcher) {
	p, l, g := fixtures(t, 0)
	p.Images = images{"python": "/img/python.ext4", "llama": "/img/llama.ext4"}
	p.Catalog = catalog{
		"python": {Name: "python", CPUs: 1, MemoryMiB: 512},
		"llama":  {Name: "llama", CPUs: 4, MemoryMiB: 4096, GPU: true, GPUVRAMMiB: 2048},
	}
	p.MaxGPUVMs = 1
	p.Start(nil)
	return &Executor{VMs: p, Guest: g, Images: p.Images, Catalog: p.Catalog}, p, l
}

// TestRunWithGPU: job com GPU numa stack com GPU sobe VM com virtio-gpu e a VRAM do
// catálogo, e a vaga de GPU volta quando a VM é destruída (o job seguinte não espera).
func TestRunWithGPU(t *testing.T) {
	e, _, l := gpuFixtures(t)
	j := job()
	j.Language, j.GPU = "llama", true
	for i := 0; i < 2; i++ { // 2 jobs com 1 vaga: só passa se a vaga voltar no Destroy
		if _, err := e.Run(context.Background(), j); err != nil {
			t.Fatalf("job %d: %v", i, err)
		}
	}
	s := l.spec(0)
	if !s.GPU || s.GPUVRAMMiB != 2048 {
		t.Fatalf("GPU job spec: %+v", s)
	}
	j.GPU = false
	if _, err := e.Run(context.Background(), j); err != nil || l.spec(2).GPU {
		t.Fatalf("CPU job on a GPU stack must not get a GPU: err=%v spec=%+v", err, l.spec(2))
	}
}

// TestRunGPUOnCPUStack: pedir GPU numa stack sem GPU é E007, sem subir VM.
func TestRunGPUOnCPUStack(t *testing.T) {
	e, _, l := gpuFixtures(t)
	j := job()
	j.GPU = true
	var ng domain.ErrNoGPU
	var ve domain.ValidationError
	if _, err := e.Run(context.Background(), j); !errors.As(err, &ng) || !errors.As(err, &ve) {
		t.Fatalf("expected E007 validation error, got %v", err)
	}
	if l.count() != 0 {
		t.Fatal("a rejected GPU job must not boot a VM")
	}
}

// TestTakeGPUSlots: com a vaga ocupada o segundo pedido espera e desiste no prazo do ctx;
// boot que falha devolve a vaga; Destroy repetido não devolve duas vezes.
func TestTakeGPUSlots(t *testing.T) {
	_, p, l := gpuFixtures(t)
	r, err := p.TakeGPU(context.Background(), "llama")
	if err != nil || !r.GPU {
		t.Fatalf("first GPU VM: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	if _, err := p.TakeGPU(ctx, "llama"); err == nil {
		t.Fatal("second GPU VM must wait for the slot and give up with the ctx")
	}
	p.Destroy(r)
	p.Destroy(r)
	if len(p.gpuSlots) != 0 {
		t.Fatalf("slots in use after destroy: %d", len(p.gpuSlots))
	}
	l.fail = errors.New("no hypervisor")
	if _, err := p.TakeGPU(context.Background(), "llama"); err == nil || len(p.gpuSlots) != 0 {
		t.Fatalf("failed boot must return the slot: err=%v slots=%d", err, len(p.gpuSlots))
	}
	l.fail = nil
	if r, err := p.TakeGPU(context.Background(), "llama"); err != nil {
		t.Fatalf("slot not free again: %v", err)
	} else {
		p.Destroy(r)
	}
}

// TestDefaultGPULimit: sem configuração o teto é DefaultMaxGPUVMs.
func TestDefaultGPULimit(t *testing.T) {
	p, _, _ := fixtures(t, 0)
	p.Start(nil)
	if cap(p.gpuSlots) != DefaultMaxGPUVMs {
		t.Fatalf("default GPU slots %d", cap(p.gpuSlots))
	}
}
