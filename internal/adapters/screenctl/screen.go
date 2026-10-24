package screenctl

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"image"
	_ "image/jpeg" // o visualizador entrega os quadros em JPEG; o DecodeConfig precisa do decodificador registrado
	"io"
	"net/http"
	"time"

	"runner.codes/internal/adapters/viewer"
)

// Screen fala com o visualizador da tela de uma máquina (o servidor HTTP que o processo da VM sobe no 127.0.0.1
// do host, na display_port).
type Screen struct {
	// Base é o endereço do visualizador, como http://127.0.0.1:6080.
	Base string
	// HTTP é o cliente usado; nil usa um com prazo curto (o visualizador é local).
	HTTP *http.Client
	// sleep é time.Sleep; os testes trocam para não esperar as pausas de verdade.
	sleep func(time.Duration)
}

// Shot é uma captura da tela: o quadro em JPEG, como o visualizador entrega, e o tamanho em pixels.
type Shot struct {
	JPEG   []byte
	Width  int
	Height int
}

// localTimeout limita cada chamada ao visualizador: ele está no mesmo host, e um quadro já pronto sai na hora.
const localTimeout = 30 * time.Second

// ForPort é o visualizador de uma máquina pela porta da tela.
func ForPort(port uint16) Screen {
	return Screen{Base: fmt.Sprintf("http://127.0.0.1:%d", port)}
}

// client devolve o cliente HTTP em uso.
func (s Screen) client() *http.Client {
	if s.HTTP != nil {
		return s.HTTP
	}
	return &http.Client{Timeout: localTimeout}
}

// Capture devolve o quadro mais recente. after=0 pede "qualquer quadro": se a tela já desenhou algo, sai na hora;
// se ainda não desenhou nada, o visualizador segura até o primeiro quadro (ou devolve 204 no prazo dele).
func (s Screen) Capture(ctx context.Context) (Shot, error) {
	req, err := http.NewRequestWithContext(ctx, "GET", s.Base+"/frame?after=0", nil)
	if err != nil {
		return Shot{}, err
	}
	resp, err := s.client().Do(req)
	if err != nil {
		return Shot{}, fmt.Errorf("screen viewer at %s: %w", s.Base, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNoContent {
		return Shot{}, fmt.Errorf("the screen has not drawn anything yet")
	}
	if resp.StatusCode != http.StatusOK {
		return Shot{}, fmt.Errorf("screen viewer: GET /frame returned %d", resp.StatusCode)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return Shot{}, err
	}
	cfg, _, err := image.DecodeConfig(bytes.NewReader(data))
	if err != nil {
		return Shot{}, fmt.Errorf("screen viewer: frame is not an image: %w", err)
	}
	return Shot{JPEG: data, Width: cfg.Width, Height: cfg.Height}, nil
}

// Do executa as ações em ordem. O tamanho da tela vem de uma captura (a tela acompanha a janela do navegador e
// muda), lida só quando alguma ação usa coordenadas. Ação inválida é recusada antes de qualquer evento sair.
func (s Screen) Do(ctx context.Context, actions []Action) error {
	w, h := 0, 0
	if needsSize(actions) {
		shot, err := s.Capture(ctx)
		if err != nil {
			return err
		}
		w, h = shot.Width, shot.Height
	}
	steps, err := Steps(actions, w, h)
	if err != nil {
		return err
	}
	sleep := s.sleep
	if sleep == nil {
		sleep = time.Sleep
	}
	for _, st := range steps {
		if len(st.Events) > 0 {
			if err := s.post(ctx, st); err != nil {
				return err
			}
		}
		sleep(st.Pause)
	}
	return nil
}

// needsSize diz se alguma ação usa coordenadas (todas, menos type, key e wait).
func needsSize(actions []Action) bool {
	for _, a := range actions {
		switch a.Action {
		case "type", "key", "wait":
		default:
			return true
		}
	}
	return false
}

// post manda um pacote ao POST /input e exige 204.
func (s Screen) post(ctx context.Context, st Step) error {
	body, _ := json.Marshal(st.Events)
	req, err := http.NewRequestWithContext(ctx, "POST", s.Base+"/input", bytes.NewReader(body))
	if err != nil {
		return err
	}
	// Marca o lote como de automação: a página do visualizador desenha a seta e o clique onde o agente age.
	req.Header.Set(viewer.AgentHeader, "1")
	resp, err := s.client().Do(req)
	if err != nil {
		return fmt.Errorf("screen viewer at %s: %w", s.Base, err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		return fmt.Errorf("screen viewer: POST /input returned %d", resp.StatusCode)
	}
	return nil
}
