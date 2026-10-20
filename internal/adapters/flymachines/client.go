// Package flymachines é o cliente da API de Machines do Fly (api.machines.dev) para o playground: cada visitante é
// uma Fly Machine própria (uma microVM do Fly), com um volume onde ficam o login do Claude e o /workspace, porque o
// disco raiz de uma Fly Machine é recriado a cada partida. Só biblioteca padrão.
package flymachines

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

// DefaultBase é a API pública de Machines.
const DefaultBase = "https://api.machines.dev/v1"

// Spec é o que toda máquina de visitante leva: imagem, região, tamanho, onde o volume é montado.
type Spec struct {
	Image string
	// Regions em ordem de preferência: falta de capacidade numa (volume_placement_capacity, medido no gru em
	// 20/10/2026) passa para a próxima; qualquer outro erro volta na hora.
	Regions  []string
	CPUKind  string // shared ou performance
	CPUs     int
	MemoryMB int
	VolumeGB int
	Mount    string // caminho do volume dentro da máquina (ex.: /data)
}

// Machine é uma Fly Machine como o playground a vê.
type Machine struct {
	ID        string            `json:"id"`
	Name      string            `json:"name"`
	State     string            `json:"state"`
	PrivateIP string            `json:"private_ip"`
	CreatedAt time.Time         `json:"created_at"`
	UpdatedAt time.Time         `json:"updated_at"`
	Metadata  map[string]string `json:"-"`
	Volume    string            `json:"-"`
}

// rawMachine é o JSON da API (metadata e mounts ficam dentro de config).
type rawMachine struct {
	Machine
	Config struct {
		Metadata map[string]string `json:"metadata"`
		Mounts   []struct {
			Volume string `json:"volume"`
		} `json:"mounts"`
	} `json:"config"`
}

// Client fala com um app do Fly (o app das máquinas de visitante) usando um token com escopo nesse app.
type Client struct {
	Base  string
	App   string
	Token string
	Spec  Spec
	HTTP  *http.Client
}

// do faz a chamada e decodifica a resposta em out (nil ignora o corpo); status fora de 2xx vira erro com o corpo.
func (c *Client) do(ctx context.Context, method, path string, in, out any) error {
	var body io.Reader
	if in != nil {
		b, err := json.Marshal(in)
		if err != nil {
			return err
		}
		body = bytes.NewReader(b)
	}
	req, err := http.NewRequestWithContext(ctx, method, c.Base+"/apps/"+url.PathEscape(c.App)+path, body)
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)
	req.Header.Set("Content-Type", "application/json")
	hc := c.HTTP
	if hc == nil {
		// 90 s: criar máquina com imagem nova inclui baixar a imagem na região; o wait tem o próprio prazo.
		hc = &http.Client{Timeout: 90 * time.Second}
	}
	resp, err := hc.Do(req)
	if err != nil {
		return fmt.Errorf("fly %s %s: %w", method, path, err)
	}
	defer resp.Body.Close()
	data, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	if resp.StatusCode/100 != 2 {
		return fmt.Errorf("fly %s %s: %d %s", method, path, resp.StatusCode, strings.TrimSpace(string(data)))
	}
	if out != nil && len(data) > 0 {
		return json.Unmarshal(data, out)
	}
	return nil
}

// fromRaw passa metadata e o primeiro volume montado para o Machine.
func fromRaw(r rawMachine) Machine {
	m := r.Machine
	m.Metadata = r.Config.Metadata
	if len(r.Config.Mounts) > 0 {
		m.Volume = r.Config.Mounts[0].Volume
	}
	return m
}

// List devolve todas as máquinas do app.
func (c *Client) List(ctx context.Context) ([]Machine, error) {
	var raw []rawMachine
	if err := c.do(ctx, http.MethodGet, "/machines", nil, &raw); err != nil {
		return nil, err
	}
	out := make([]Machine, 0, len(raw))
	for _, r := range raw {
		out = append(out, fromRaw(r))
	}
	return out, nil
}

// Create cria o volume e a máquina com o nome dado, a env e a metadata; a máquina nasce ligada. Tenta as regiões em
// ordem e só segue para a próxima quando a recusa é de capacidade.
func (c *Client) Create(ctx context.Context, name string, env, metadata map[string]string) (Machine, error) {
	var err error
	for _, region := range c.Spec.Regions {
		var m Machine
		m, err = c.createIn(ctx, region, name, env, metadata)
		if err == nil || !isCapacity(err) {
			return m, err
		}
	}
	if err == nil {
		err = fmt.Errorf("fly: no region configured")
	}
	return Machine{}, err
}

// isCapacity reconhece a recusa por falta de recurso no host ou na região (a API devolve 412 ou 422 com estes textos).
func isCapacity(err error) bool {
	s := err.Error()
	return strings.Contains(s, "insufficient resources") || strings.Contains(s, "capacity")
}

// createIn cria o volume e a máquina numa região; se a máquina não nasce, o volume é apagado.
func (c *Client) createIn(ctx context.Context, region, name string, env, metadata map[string]string) (Machine, error) {
	var vol struct {
		ID string `json:"id"`
	}
	// Nome de volume do Fly: letras minúsculas, números e sublinhado.
	volName := strings.ReplaceAll(name, "-", "_")
	if err := c.do(ctx, http.MethodPost, "/volumes", map[string]any{"name": volName, "size_gb": c.Spec.VolumeGB, "region": region}, &vol); err != nil {
		return Machine{}, err
	}
	body := map[string]any{
		"name":   name,
		"region": region,
		"config": map[string]any{
			"image":    c.Spec.Image,
			"env":      env,
			"metadata": metadata,
			"guest":    map[string]any{"cpu_kind": c.Spec.CPUKind, "cpus": c.Spec.CPUs, "memory_mb": c.Spec.MemoryMB},
			"mounts":   []map[string]any{{"volume": vol.ID, "path": c.Spec.Mount}},
			// Sem reinício automático: quem liga e desliga é o playground (ociosidade e teto de máquinas).
			"restart":      map[string]any{"policy": "no"},
			"auto_destroy": false,
		},
	}
	var raw rawMachine
	if err := c.do(ctx, http.MethodPost, "/machines", body, &raw); err != nil {
		// Máquina que não nasceu não deixa volume órfão cobrando.
		c.do(context.Background(), http.MethodDelete, "/volumes/"+vol.ID, nil, nil)
		return Machine{}, err
	}
	return fromRaw(raw), nil
}

// Start liga uma máquina parada.
func (c *Client) Start(ctx context.Context, id string) error {
	return c.do(ctx, http.MethodPost, "/machines/"+id+"/start", nil, nil)
}

// Stop desliga a máquina (o volume fica).
func (c *Client) Stop(ctx context.Context, id string) error {
	return c.do(ctx, http.MethodPost, "/machines/"+id+"/stop", nil, nil)
}

// Wait espera a máquina chegar ao estado (started, stopped) por até timeout segundos (a API aceita até 60).
func (c *Client) Wait(ctx context.Context, id, state string, timeout int) error {
	return c.do(ctx, http.MethodGet, fmt.Sprintf("/machines/%s/wait?state=%s&timeout=%d", id, url.QueryEscape(state), timeout), nil, nil)
}

// Delete apaga a máquina e o volume dela.
func (c *Client) Delete(ctx context.Context, m Machine) error {
	if err := c.do(ctx, http.MethodDelete, "/machines/"+m.ID+"?force=true", nil, nil); err != nil {
		return err
	}
	if m.Volume != "" {
		return c.do(ctx, http.MethodDelete, "/volumes/"+m.Volume, nil, nil)
	}
	return nil
}
