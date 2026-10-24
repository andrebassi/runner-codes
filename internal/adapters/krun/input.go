package krun

import (
	"os"
	"sync"
	"time"
	"unsafe"

	"github.com/ebitengine/purego"

	"runner.codes/internal/adapters/viewer"
)

// Códigos do evdev (linux/input-event-codes.h) que os dispositivos declaram.
const (
	evSyn      = 0x00
	evKey      = 0x01
	evRel      = 0x02
	evAbs      = 0x03
	relWheel   = 0x08
	absX       = 0x00
	absY       = 0x01
	btnLeft    = 0x110
	btnMiddle  = 0x112
	busVirtual = 0x06
	// absMax é o fim do eixo absoluto do tablet: o navegador manda a posição já em 0..32767,
	// independente do tamanho da tela (o guest escala para os pixels dele).
	absMax = 32767
	// maxKeyCode é o maior keycode que o visualizador manda (KEY_RIGHTMETA = 126); o bitmap do
	// teclado cobre 0..maxKeyCode.
	maxKeyCode = 126
)

// Flags de libkrun_input.h que a libkrun 1.19 exige nos dois objetos.
const (
	inputFeatureQuery = 1
	inputFeatureQueue = 1
)

// IDs dos dois dispositivos: o valor vai no userdata e volta como instance nos callbacks.
const (
	inputKeyboard = 1
	inputTablet   = 2
)

// inputEvent espelha struct krun_input_event.
type inputEvent struct {
	Type  uint16
	Code  uint16
	Value uint32
}

// inputIDs espelha struct krun_input_device_ids.
type inputIDs struct{ Bustype, Vendor, Product, Version uint16 }

// absInfo espelha struct krun_input_absinfo.
type absInfo struct{ Min, Max, Fuzz, Flat, Res uint32 }

// inputConfig espelha struct krun_input_config.
type inputConfig struct {
	features uint64
	userdata uintptr
	create   uintptr
	destroy  uintptr
	name     uintptr
	serial   uintptr
	ids      uintptr
	caps     uintptr
	abs      uintptr
	props    uintptr
}

// inputProvider espelha struct krun_input_event_provider.
type inputProvider struct {
	features uint64
	userdata uintptr
	create   uintptr
	destroy  uintptr
	readyFD  uintptr
	next     uintptr
}

// inputQueue é a fila de eventos de um dispositivo, com um pipe que fica legível enquanto
// há evento: é o "efd" que a libkrun espera (no Linux seria eventfd; o pipe vale nos dois).
type inputQueue struct {
	mu     sync.Mutex
	events []inputEvent
	r, w   *os.File
	armed  bool
	// next é quando o próximo pacote pode sair (cadência entre SYN_REPORTs).
	next time.Time
	// waiting diz que um timer já vai rearmar o pipe quando a cadência liberar.
	waiting bool
}

// packetGap é o intervalo mínimo entre dois pacotes (um pacote termina no SYN_REPORT). Sem
// ele uma rajada de teclas (o navegador colando 40 caracteres) chega ao guest de uma vez e
// estoura o buffer de 64 eventos do evdev de cada cliente: o Weston acusou SYN_DROPPED e só a
// 1ª letra chegou (medido). 3 ms por pacote = ~240 ms para 40 caracteres.
const packetGap = 3 * time.Millisecond

// push põe eventos na fila e arma o pipe (um byte basta para acordar o leitor).
func (q *inputQueue) push(evs []inputEvent) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.events = append(q.events, evs...)
	if !q.armed && len(q.events) > 0 {
		q.w.Write([]byte{1})
		q.armed = true
	}
}

// disarm esvazia o byte do pipe (com o lock).
func (q *inputQueue) disarm() {
	if q.armed {
		var b [1]byte
		q.r.Read(b[:])
		q.armed = false
	}
}

// pop tira o próximo evento. Com a fila vazia, ou dentro do intervalo entre pacotes, desarma o
// pipe (o poll da libkrun não gira em falso) e, se ainda há evento, agenda o rearme para quando
// a cadência liberar.
func (q *inputQueue) pop() (inputEvent, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if len(q.events) == 0 {
		q.disarm()
		return inputEvent{}, false
	}
	if wait := time.Until(q.next); wait > 0 {
		q.disarm()
		if !q.waiting {
			q.waiting = true
			time.AfterFunc(wait, func() {
				q.mu.Lock()
				defer q.mu.Unlock()
				q.waiting = false
				if !q.armed && len(q.events) > 0 {
					q.w.Write([]byte{1})
					q.armed = true
				}
			})
		}
		return inputEvent{}, false
	}
	ev := q.events[0]
	q.events = q.events[1:]
	if ev.Type == evSyn {
		q.next = time.Now().Add(packetGap)
	}
	return ev, true
}

// inputs são as duas filas, por id de dispositivo; um processo vmm tem uma VM só.
var (
	inputs    = map[uintptr]*inputQueue{}
	inputOnce sync.Once
	inputCfg  [2]*inputConfig
	inputEvs  [2]*inputProvider
)

// newInputQueue cria a fila com o pipe.
func newInputQueue() (*inputQueue, error) {
	r, w, err := os.Pipe()
	if err != nil {
		return nil, err
	}
	return &inputQueue{r: r, w: w}, nil
}

// newInputBackends prepara teclado e tablet e devolve as estruturas para a
// krun_add_input_device, e o Input que o visualizador usa para entregar os eventos.
func newInputBackends() ([2]*inputConfig, [2]*inputProvider, viewer.Input, error) {
	var err error
	inputOnce.Do(func() {
		create := purego.NewCallback(inputCreate)
		name, serial := purego.NewCallback(inputName), purego.NewCallback(inputSerial)
		ids, caps := purego.NewCallback(inputDeviceIDs), purego.NewCallback(inputCaps)
		abs, props := purego.NewCallback(inputAbs), purego.NewCallback(inputProps)
		ready, next := purego.NewCallback(inputReadyFD), purego.NewCallback(inputNext)
		for i, id := range []uintptr{inputKeyboard, inputTablet} {
			// features: KRUN_INPUT_CONFIG_FEATURE_QUERY e KRUN_INPUT_EVENT_PROVIDER_FEATURE_QUEUE
			// (os dois valem 1); sem eles a libkrun recusa o dispositivo com EINVAL (medido).
			inputCfg[i] = &inputConfig{features: inputFeatureQuery, userdata: id, create: create, name: name, serial: serial, ids: ids, caps: caps, abs: abs, props: props}
			inputEvs[i] = &inputProvider{features: inputFeatureQueue, userdata: id, create: create, readyFD: ready, next: next}
		}
	})
	for _, id := range []uintptr{inputKeyboard, inputTablet} {
		if inputs[id], err = newInputQueue(); err != nil {
			return inputCfg, inputEvs, nil, err
		}
	}
	return inputCfg, inputEvs, router{}, nil
}

// router reparte os eventos do navegador: tecla vai ao teclado; botão, eixo e roda ao
// tablet. Cada SYN_REPORT do cliente fecha um pacote em cada dispositivo — o lote do navegador
// junta várias teclas, e o libevdev do guest agrega por pacote: "A desce, A sobe" no mesmo
// pacote se anula e a tecla some (só a 1ª letra de uma frase chegava, medido).
type router struct{}

// Push entrega o lote, pacote a pacote.
func (router) Push(evs []viewer.Event) {
	var kbd, tab []inputEvent
	syn := inputEvent{Type: evSyn}
	flush := func() {
		if len(kbd) > 0 {
			inputs[inputKeyboard].push(append(kbd, syn))
		}
		if len(tab) > 0 {
			inputs[inputTablet].push(append(tab, syn))
		}
		kbd, tab = nil, nil
	}
	for _, e := range evs {
		ev := inputEvent{Type: e.Type, Code: e.Code, Value: uint32(e.Value)}
		switch {
		case e.Type == evSyn:
			flush()
		case e.Type == evKey && e.Code < btnLeft:
			kbd = append(kbd, ev)
		case e.Type == evKey || e.Type == evAbs || e.Type == evRel:
			tab = append(tab, ev)
		}
	}
	flush() // lote sem SYN no fim (cliente que não manda) ainda fecha o pacote
}

// inputCreate devolve o id do dispositivo como instância.
func inputCreate(instance *uintptr, userdata uintptr, _ uintptr) int32 {
	if instance != nil {
		*instance = userdata
	}
	return 0
}

// writeName copia s para o buffer do C e devolve quantos bytes escreveu.
func writeName(buf *byte, n uintptr, s string) int32 {
	if buf == nil {
		return 0
	}
	return int32(copy(unsafe.Slice(buf, n), s))
}

// inputName dá o nome que aparece no guest (/proc/bus/input/devices, libinput list-devices).
func inputName(inst uintptr, buf *byte, n uintptr) int32 {
	if inst == inputKeyboard {
		return writeName(buf, n, "runner.code keyboard")
	}
	return writeName(buf, n, "runner.code tablet")
}

// inputSerial dá um número de série estável por dispositivo.
func inputSerial(inst uintptr, buf *byte, n uintptr) int32 {
	if inst == inputKeyboard {
		return writeName(buf, n, "rc-kbd-0")
	}
	return writeName(buf, n, "rc-tab-0")
}

// inputDeviceIDs declara barramento virtual e um id por dispositivo.
func inputDeviceIDs(inst uintptr, ids *inputIDs) int32 {
	if ids != nil {
		*ids = inputIDs{Bustype: busVirtual, Vendor: 0x5243, Product: uint16(inst), Version: 1}
	}
	return 0
}

// setBit liga o bit code no bitmap e devolve o tamanho usado até ele.
func setBit(bm []byte, code int) int {
	if code/8 >= len(bm) {
		return 0
	}
	bm[code/8] |= 1 << (code % 8)
	return code/8 + 1
}

// inputCaps declara o que cada dispositivo emite: teclado = keycodes 1..maxKeyCode; tablet =
// botões, eixos X/Y absolutos e a roda. Devolve quantos bytes do bitmap valem.
func inputCaps(inst uintptr, evType uint8, buf *byte, n uintptr) int32 {
	if buf == nil {
		return 0
	}
	bm := unsafe.Slice(buf, n)
	used := 0
	mark := func(code int) { used = max(used, setBit(bm, code)) }
	switch {
	case inst == inputKeyboard && evType == evKey:
		for c := 1; c <= maxKeyCode; c++ {
			mark(c)
		}
	case inst == inputTablet && evType == evKey:
		for c := btnLeft; c <= btnMiddle; c++ {
			mark(c)
		}
	case inst == inputTablet && evType == evAbs:
		mark(absX)
		mark(absY)
	case inst == inputTablet && evType == evRel:
		mark(relWheel)
	}
	return int32(used)
}

// inputAbs dá a faixa dos eixos absolutos do tablet.
func inputAbs(_ uintptr, axis uint8, info *absInfo) int32 {
	if info != nil {
		*info = absInfo{}
		if axis == absX || axis == absY {
			info.Max = absMax
		}
	}
	return 0
}

// inputProps não declara propriedades (nem INPUT_PROP_DIRECT: é um ponteiro, não uma tela
// de toque).
func inputProps(uintptr, *byte, uintptr) int32 { return 0 }

// inputReadyFD devolve o lado de leitura do pipe da fila do dispositivo.
func inputReadyFD(inst uintptr) int32 { return int32(inputs[inst].r.Fd()) }

// inputNext entrega o próximo evento (1) ou diz que acabou (0).
func inputNext(inst uintptr, out *inputEvent) int32 {
	ev, ok := inputs[inst].pop()
	if !ok || out == nil {
		return 0
	}
	dlog("input next dev=%d type=%d code=%d value=%d", inst, ev.Type, ev.Code, int32(ev.Value))
	*out = ev
	return 1
}
