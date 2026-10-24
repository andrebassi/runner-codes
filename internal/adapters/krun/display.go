package krun

import (
	"fmt"
	"os"
	"sync"
	"syscall"
	"unsafe"

	"github.com/ebitengine/purego"

	"runner.codes/internal/adapters/screen"
)

// Constantes de libkrun_display.h.
const (
	displayFeatureBasicFramebuffer = 1
	displayErrInvalidScanout       = -3
	displayErrInvalidParam         = -4
	displayErrOutOfBuffers         = -5
	// framesPerScanout: dois buffers bastam (um sendo escrito pelo guest, um sendo copiado
	// para a tela); o present devolve o frame na hora, então o anel nunca enche na prática.
	framesPerScanout = 2
)

// krunRect espelha struct krun_rect.
type krunRect struct{ X, Y, Width, Height uint32 }

// displayBackend espelha struct krun_display_backend: features, userdata, create e a vtable
// basic_framebuffer (destroy, disable_scanout, configure_scanout, alloc_frame, present_frame).
// Os campos são ponteiros de função C (uintptr), na ordem do header.
type displayBackend struct {
	features  uint64
	userdata  uintptr
	create    uintptr
	destroy   uintptr
	disable   uintptr
	configure uintptr
	alloc     uintptr
	present   uintptr
}

// scanout é uma saída de vídeo configurada pelo guest, com os seus buffers.
type scanout struct {
	width, height int
	format        uint32
	frames        [framesPerScanout][]byte // memória mmap: endereço fixo, fora do GC
	busy          [framesPerScanout]bool
}

// framebuffer é o backend: recebe os frames do guest e os entrega à tela.
type framebuffer struct {
	mu       sync.Mutex
	scanouts map[uint32]*scanout
	screen   *screen.Screen
}

// O backend é um só por processo (uma VM por processo vmm): callbacks C não carregam
// contexto Go, então o estado vive aqui.
var (
	fbState   *framebuffer
	fbBackend *displayBackend
	fbOnce    sync.Once
)

// newDisplayBackend prepara a vtable apontando para a tela dada e devolve o ponteiro para
// passar à krun_set_display_backend. Os callbacks são criados uma vez só: o purego não libera
// callback, e o limite do processo é ~2000.
func newDisplayBackend(s *screen.Screen) *displayBackend {
	fbOnce.Do(func() {
		fbBackend = &displayBackend{
			features:  displayFeatureBasicFramebuffer,
			create:    purego.NewCallback(displayCreate),
			disable:   purego.NewCallback(displayDisable),
			configure: purego.NewCallback(displayConfigure),
			alloc:     purego.NewCallback(displayAlloc),
			present:   purego.NewCallback(displayPresent),
		}
	})
	fbState = &framebuffer{scanouts: map[uint32]*scanout{}, screen: s}
	return fbBackend
}

// displayDebug liga o log de cada chamada da libkrun ao backend (RUNNER_DISPLAY_DEBUG=1), no
// stderr do processo vmm (o vmm.log da máquina): é como se separa "nunca chamou" de "falhou".
var displayDebug = os.Getenv("RUNNER_DISPLAY_DEBUG") == "1"

// dlog escreve uma linha de depuração do display.
func dlog(format string, args ...any) {
	if displayDebug {
		fmt.Fprintf(os.Stderr, "display: "+format+"\n", args...)
	}
}

// displaySize é o sizeof(struct krun_display_backend) que a libkrun confere.
var displaySize = unsafe.Sizeof(displayBackend{})

// displayCreate não precisa de instância: o estado é o fbState.
func displayCreate(instance *uintptr, _ uintptr, _ uintptr) int32 {
	dlog("create")
	if instance != nil {
		*instance = 0
	}
	return 0
}

// displayDisable desliga a saída e devolve os buffers.
func displayDisable(_ uintptr, id uint32) int32 { return fbState.disable(id) }

// displayConfigure (re)cria a saída com o tamanho e o formato pedidos.
func displayConfigure(_ uintptr, id, dw, dh, w, h, format uint32) int32 {
	rc := fbState.configure(id, int(w), int(h), format)
	dlog("configure scanout=%d display=%dx%d scanout=%dx%d format=%d rc=%d", id, dw, dh, w, h, format, rc)
	return rc
}

// displayAlloc entrega ao guest um buffer livre para ele desenhar o próximo frame.
func displayAlloc(_ uintptr, id uint32, buffer *uintptr, size *uintptr) int32 {
	return fbState.alloc(id, buffer, size)
}

// displayPresent publica o frame na tela e o devolve ao anel.
func displayPresent(_ uintptr, id, frame uint32, _ *krunRect) int32 {
	rc := fbState.present(id, frame)
	dlog("present scanout=%d frame=%d rc=%d", id, frame, rc)
	return rc
}

// configure troca os buffers da saída pelos do tamanho novo.
func (f *framebuffer) configure(id uint32, w, h int, format uint32) int32 {
	if w <= 0 || h <= 0 || w*h > 8192*8192 {
		return displayErrInvalidParam
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.release(id)
	so := &scanout{width: w, height: h, format: format}
	for i := range so.frames {
		buf, err := syscall.Mmap(-1, 0, w*h*4, syscall.PROT_READ|syscall.PROT_WRITE, syscall.MAP_ANON|syscall.MAP_PRIVATE)
		if err != nil {
			return displayErrOutOfBuffers
		}
		so.frames[i] = buf
	}
	f.scanouts[id] = so
	return 0
}

// disable desliga a saída.
func (f *framebuffer) disable(id uint32) int32 {
	f.mu.Lock()
	defer f.mu.Unlock()
	if _, ok := f.scanouts[id]; !ok {
		return displayErrInvalidScanout
	}
	f.release(id)
	return 0
}

// release devolve a memória dos buffers da saída (com o lock).
func (f *framebuffer) release(id uint32) {
	if so, ok := f.scanouts[id]; ok {
		for _, b := range so.frames {
			if b != nil {
				syscall.Munmap(b)
			}
		}
		delete(f.scanouts, id)
	}
}

// alloc marca um buffer como ocupado e escreve endereço e tamanho onde o C pediu.
func (f *framebuffer) alloc(id uint32, buffer *uintptr, size *uintptr) int32 {
	f.mu.Lock()
	defer f.mu.Unlock()
	so, ok := f.scanouts[id]
	if !ok {
		return displayErrInvalidScanout
	}
	for i, busy := range so.busy {
		if !busy {
			so.busy[i] = true
			if buffer != nil {
				*buffer = uintptr(unsafe.Pointer(&so.frames[i][0]))
			}
			if size != nil {
				*size = uintptr(len(so.frames[i]))
			}
			return int32(i)
		}
	}
	return displayErrOutOfBuffers
}

// present copia o frame para a tela e libera o buffer.
func (f *framebuffer) present(id, frame uint32) int32 {
	f.mu.Lock()
	so, ok := f.scanouts[id]
	if !ok || int(frame) >= framesPerScanout || !so.busy[frame] {
		f.mu.Unlock()
		if !ok {
			return displayErrInvalidScanout
		}
		return displayErrInvalidParam
	}
	data, w, h, format := so.frames[frame], so.width, so.height, so.format
	f.mu.Unlock()
	f.screen.Update(w, h, format, data)
	f.mu.Lock()
	so.busy[frame] = false
	f.mu.Unlock()
	return 0
}
