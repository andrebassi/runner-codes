package krun

import (
	"strings"
	"testing"
	"time"
	"unsafe"

	"runner.codes/internal/adapters/screen"
	"runner.codes/internal/adapters/viewer"
	"runner.codes/internal/ports"
)

// TestDisplayBackend: o ciclo que a libkrun faz (configure, alloc, o guest escreve, present)
// chega à tela; os dois buffers giram; os erros têm o código da libkrun_display.h.
func TestDisplayBackend(t *testing.T) {
	scr := screen.New()
	be := newDisplayBackend(scr)
	if be.features != displayFeatureBasicFramebuffer || be.present == 0 || displaySize != 64 {
		t.Fatalf("backend: %+v size %d", be, displaySize)
	}
	var inst uintptr = 99
	if displayCreate(&inst, 0, 0) != 0 || inst != 0 || displayCreate(nil, 0, 0) != 0 {
		t.Fatal("create")
	}
	if displayConfigure(0, 0, 2, 1, 2, 1, screen.FormatR8G8B8A8) != 0 {
		t.Fatal("configure")
	}
	var buf, size uintptr
	id := displayAlloc(0, 0, &buf, &size)
	if id != 0 || size != 8 || buf == 0 {
		t.Fatalf("alloc: %d %d", id, size)
	}
	// O guest escreveria pelo endereço; o teste escreve no mesmo buffer pelo slice do backend
	// e confere que o endereço entregue é o dele.
	frame := fbState.scanouts[0].frames[id]
	if uintptr(unsafe.Pointer(&frame[0])) != buf {
		t.Fatal("alloc must hand out the frame's own address")
	}
	copy(frame, []byte{10, 20, 30, 0, 40, 50, 60, 0})
	if id2 := displayAlloc(0, 0, nil, nil); id2 != 1 {
		t.Fatalf("second buffer: %d", id2)
	}
	// Os dois ocupados = quadros abandonados por flush que falhou (troca de modo no guest): a
	// reserva seguinte os recicla em vez de recusar, senão a tela congelava para sempre.
	if id3 := displayAlloc(0, 0, nil, nil); id3 != 0 || !fbState.scanouts[0].busy[0] || fbState.scanouts[0].busy[1] {
		t.Fatalf("both busy must recycle: %d %v", id3, fbState.scanouts[0].busy)
	}
	if displayPresent(0, 0, 0, nil) != 0 {
		t.Fatal("present")
	}
	img, seq := scr.Snapshot(0, time.Second)
	if seq != 1 || img.Pix[0] != 10 || img.Pix[6] != 60 {
		t.Fatalf("frame on screen: %v", img.Pix)
	}
	for name, rc := range map[string]int32{
		"present twice":   displayPresent(0, 0, 0, nil),
		"bad frame":       displayPresent(0, 0, 9, nil),
		"bad scanout":     displayPresent(0, 5, 0, nil),
		"alloc unknown":   displayAlloc(0, 5, nil, nil),
		"disable unknown": displayDisable(0, 5),
		"zero size":       displayConfigure(0, 1, 0, 0, 0, 0, 1),
	} {
		if rc >= 0 {
			t.Fatalf("%s: rc %d", name, rc)
		}
	}
	if displayDisable(0, 0) != 0 {
		t.Fatal("disable")
	}
}

// TestInputQueues: o roteador manda tecla ao teclado e botão/eixo/roda ao tablet, com SYN no
// fim; o pipe fica legível enquanto há evento e desarma ao esvaziar; as consultas de config
// devolvem nomes, ids, capacidades e eixos.
func TestInputQueues(t *testing.T) {
	_, _, in, err := newInputBackends()
	if err != nil {
		t.Fatal(err)
	}
	if inputCfg[0].features != inputFeatureQuery || inputEvs[1].features != inputFeatureQueue {
		t.Fatal("libkrun refuses input devices without the QUERY/QUEUE features")
	}
	in.Push([]viewer.Event{{Type: evKey, Code: 30, Value: 1}, {Type: evAbs, Code: absX, Value: 100}, {Type: evKey, Code: btnLeft, Value: 1}, {Type: evRel, Code: relWheel, Value: -1}, {Type: 9}})
	var ev inputEvent
	var kbd, tab []inputEvent
	// drain lê como a libkrun: até a fila dizer que não há mais nada agora.
	drain := func(id uintptr, out *[]inputEvent) {
		for inputNext(id, &ev) == 1 {
			*out = append(*out, ev)
		}
	}
	drain(inputKeyboard, &kbd)
	drain(inputTablet, &tab)
	if len(kbd) != 2 || kbd[0].Code != 30 || kbd[1].Type != evSyn || len(tab) != 4 || tab[2].Value != uint32(0xffffffff) || tab[3].Type != evSyn {
		t.Fatalf("routing: kbd=%+v tab=%+v", kbd, tab)
	}
	if inputNext(inputTablet, nil) != 0 || inputReadyFD(inputKeyboard) <= 0 {
		t.Fatal("empty queue / fd")
	}
	name := make([]byte, 64)
	if n := inputName(inputKeyboard, &name[0], 64); !strings.HasPrefix(string(name[:n]), "runner.code keyboard") {
		t.Fatalf("name: %q", name[:n])
	}
	if n := inputName(inputTablet, &name[0], 64); string(name[:n]) != "runner.code tablet" {
		t.Fatal("tablet name")
	}
	if inputSerial(inputKeyboard, &name[0], 64) == 0 || inputSerial(inputTablet, &name[0], 64) == 0 || writeName(nil, 0, "x") != 0 {
		t.Fatal("serial")
	}
	var ids inputIDs
	if inputDeviceIDs(inputTablet, &ids) != 0 || ids.Bustype != busVirtual || ids.Product != inputTablet || inputDeviceIDs(1, nil) != 0 {
		t.Fatalf("ids: %+v", ids)
	}
	bm := make([]byte, 128)
	if n := inputCaps(inputKeyboard, evKey, &bm[0], 128); n != maxKeyCode/8+1 || bm[30/8]&(1<<(30%8)) == 0 {
		t.Fatalf("keyboard caps: %d", n)
	}
	for typ, want := range map[uint8]int32{evKey: btnMiddle/8 + 1, evAbs: 1, evRel: 2, evSyn: 0} {
		clear(bm)
		if n := inputCaps(inputTablet, typ, &bm[0], 128); n != want {
			t.Fatalf("tablet caps type %d: %d", typ, n)
		}
	}
	if inputCaps(1, evKey, nil, 0) != 0 || setBit(make([]byte, 1), 100) != 0 {
		t.Fatal("caps edges")
	}
	var info absInfo
	if inputAbs(0, absY, &info) != 0 || info.Max != absMax || inputAbs(0, 9, &info) != 0 || info.Max != 0 || inputAbs(0, 0, nil) != 0 {
		t.Fatalf("abs: %+v", info)
	}
	var instance uintptr
	if inputCreate(&instance, inputTablet, 0) != 0 || instance != inputTablet || inputCreate(nil, 1, 0) != 0 || inputProps(0, nil, 0) != 0 {
		t.Fatal("create/props")
	}
}

// TestConfigureDisplay: com display e tela, a VM ganha a saída e os dois dispositivos de
// entrada; a spec recusa display sem GPU, sem endereço ou grande demais; o startDisplay
// recusa endereço ocupado.
func TestConfigureDisplay(t *testing.T) {
	newInputBackends()
	f := &fakeAPI{}
	s := jobSpec()
	s.GPU, s.Display = true, &ports.VMDisplay{Width: 800, Height: 600, Listen: "127.0.0.1:0"}
	if _, err := configureFull(f, s, -1, screen.New()); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(f.calls, ",")
	if !strings.Contains(got, "krun_add_display,krun_set_display_backend,krun_add_input_device,krun_add_input_device") {
		t.Fatalf("calls: %s", got)
	}
	for name, d := range map[string]*ports.VMDisplay{"no listen": {Width: 1, Height: 1}, "huge": {Width: 9000, Height: 1, Listen: "x"}} {
		bad := jobSpec()
		bad.GPU, bad.Display = true, d
		if bad.Validate() == nil {
			t.Fatalf("%s must be refused", name)
		}
	}
	nogpu := jobSpec()
	nogpu.Display = &ports.VMDisplay{Width: 1, Height: 1, Listen: "x"}
	if nogpu.Validate() == nil {
		t.Fatal("display without gpu must be refused")
	}
	if scr, err := startDisplay(nil, ""); scr != nil || err != nil {
		t.Fatal("no display, no screen")
	}
	if _, err := startDisplay(&ports.VMDisplay{Width: 1, Height: 1, Listen: "256.0.0.1:1"}, ""); err == nil {
		t.Fatal("bad listen must fail")
	}
	scr, err := startDisplay(&ports.VMDisplay{Width: 1, Height: 1, Listen: "127.0.0.1:0"}, "")
	if err != nil || scr == nil {
		t.Fatal(err)
	}
}

// TestInputPacing: dois pacotes seguidos não saem juntos — depois do SYN_REPORT a fila espera
// packetGap, desarma o pipe e o rearma sozinha quando libera; nada se perde.
func TestInputPacing(t *testing.T) {
	q, err := newInputQueue()
	if err != nil {
		t.Fatal(err)
	}
	syn := inputEvent{Type: evSyn}
	q.push([]inputEvent{{Type: evKey, Code: 30, Value: 1}, syn, {Type: evKey, Code: 30}, syn})
	var got []inputEvent
	for {
		ev, ok := q.pop()
		if !ok {
			break
		}
		got = append(got, ev)
	}
	q.mu.Lock()
	armed := q.armed
	q.mu.Unlock()
	if len(got) != 2 || armed {
		t.Fatalf("only the first packet may leave now, and the pipe must be disarmed: %+v armed=%v", got, armed)
	}
	buf := make([]byte, 1)
	q.r.SetReadDeadline(time.Now().Add(time.Second))
	if n, err := q.r.Read(buf); n != 1 || err != nil {
		t.Fatalf("the timer must rearm the pipe: %v", err)
	}
	q.mu.Lock()
	q.armed = false // o byte já foi lido pelo teste
	q.mu.Unlock()
	for {
		ev, ok := q.pop()
		if !ok {
			break
		}
		got = append(got, ev)
	}
	if len(got) != 4 || got[2].Value != 0 {
		t.Fatalf("the second packet must arrive after the gap: %+v", got)
	}
}

// TestRouterKeepsPackets: um lote com várias teclas, cada uma fechada por SYN, sai em pacotes
// separados — nunca "A desce, A sobe" no mesmo pacote, que o libevdev do guest anula.
func TestRouterKeepsPackets(t *testing.T) {
	_, _, in, _ := newInputBackends()
	syn := viewer.Event{Type: evSyn}
	in.Push([]viewer.Event{{Type: evKey, Code: 30, Value: 1}, syn, {Type: evKey, Code: 30}, syn, {Type: evKey, Code: 48, Value: 1}, syn, {Type: evKey, Code: 48}})
	q := inputs[inputKeyboard]
	q.mu.Lock()
	evs := append([]inputEvent{}, q.events...)
	q.mu.Unlock()
	var packets [][]uint16
	cur := []uint16{}
	for _, e := range evs {
		if e.Type == evSyn {
			packets = append(packets, cur)
			cur = []uint16{}
			continue
		}
		cur = append(cur, e.Code)
	}
	if len(packets) != 4 {
		t.Fatalf("each key event must be its own packet: %v", packets)
	}
	for _, p := range packets {
		if len(p) != 1 {
			t.Fatalf("a packet with more than one key event: %v", packets)
		}
	}
}
