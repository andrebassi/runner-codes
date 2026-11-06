package client

import (
	"context"
	"io"
	"net/http"
	"net/url"
)

// TerminalOptions é o pedido de um terminal novo: o usuário (vazio é o da máquina) e o tamanho inicial (0 é 32×120).
type TerminalOptions struct {
	User string `json:"user,omitempty"`
	Rows uint16 `json:"rows,omitempty"`
	Cols uint16 `json:"cols,omitempty"`
}

// terminalPath monta /api/v1/machines/<nome>/terminal/<id><sufixo>.
func terminalPath(name, id, suffix string) string {
	return machinePath(name, "/terminal/"+url.PathEscape(id)+suffix)
}

// OpenTerminal liga o shell padrão da imagem num PTY da máquina ligada e devolve o id da sessão (POST
// .../terminal). A sessão vive no servidor até o CloseTerminal ou 10 min sem ninguém olhando.
func (c *Client) OpenTerminal(ctx context.Context, name string, o TerminalOptions) (string, error) {
	var out struct {
		ID string `json:"id"`
	}
	err := c.doJSON(ctx, http.MethodPost, machinePath(name, "/terminal"), o, &out)
	return out.ID, err
}

// TerminalStream abre a SSE da tela do terminal (GET .../terminal/:id/stream): um evento replay com a tela recente,
// um data por pedaço novo (base64) e um exit com o código. Quem chama lê o corpo e o fecha. Sem o prazo do cliente:
// a SSE dura o que a aba durar, e o fim vem do ctx.
func (c *Client) TerminalStream(ctx context.Context, name, id string) (io.ReadCloser, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, c.BaseURL+terminalPath(name, id, "/stream"), nil)
	if err != nil {
		return nil, err
	}
	if c.Token != "" {
		req.Header.Set("Authorization", "Bearer "+c.Token)
	}
	hc := &http.Client{}
	if c.HTTP != nil {
		hc.Transport = c.HTTP.Transport
	}
	resp, err := hc.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		defer resp.Body.Close()
		data, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<16))
		return nil, decodeError(resp.StatusCode, data)
	}
	return resp.Body, nil
}

// TerminalInput manda teclado (ou colagem) ao PTY (POST .../terminal/:id/input), em bytes crus.
func (c *Client) TerminalInput(ctx context.Context, name, id string, data io.Reader) error {
	return c.do(ctx, http.MethodPost, terminalPath(name, id, "/input"), data, "application/octet-stream", nil)
}

// TerminalResize repassa o tamanho da janela ao PTY (POST .../terminal/:id/resize).
func (c *Client) TerminalResize(ctx context.Context, name, id string, rows, cols uint16) error {
	return c.doJSON(ctx, http.MethodPost, terminalPath(name, id, "/resize"), map[string]uint16{"rows": rows, "cols": cols}, nil)
}

// CloseTerminal encerra o shell e esquece a sessão (DELETE .../terminal/:id).
func (c *Client) CloseTerminal(ctx context.Context, name, id string) error {
	return c.do(ctx, http.MethodDelete, terminalPath(name, id, ""), nil, "", nil)
}
