package client

import (
	"context"
	"fmt"
	"net/http"
)

// EgressEvent é uma linha do registro do proxy de saída do host: um request HTTP(S), uma conexão TLS que não foi
// aberta ou uma conexão TCP opaca. Nunca traz header nem corpo; Target é o caminho com a query sem valores.
type EgressEvent struct {
	Seq        uint64  `json:"seq"`
	AtMs       int64   `json:"atMs"`
	Machine    string  `json:"machine"`
	Kind       string  `json:"kind"`
	Verdict    string  `json:"verdict"`
	Host       string  `json:"host"`
	Port       int     `json:"port"`
	Method     *string `json:"method"`
	Target     *string `json:"target"`
	Status     *int    `json:"status"`
	BytesUp    int64   `json:"bytesUp"`
	BytesDown  int64   `json:"bytesDown"`
	DurationMs int64   `json:"durationMs"`
	Note       *string `json:"note"`
}

// EgressPage é uma leitura do registro: Epoch muda quando o proxy reinicia (o cursor recomeça), Next é o cursor da
// próxima leitura, e Lost diz que eventos depois do cursor pedido já tinham saído do buffer.
type EgressPage struct {
	Epoch  uint64        `json:"epoch"`
	Next   uint64        `json:"next"`
	Lost   bool          `json:"lost"`
	Events []EgressEvent `json:"events"`
}

// EgressEvents lê o registro do proxy de saída do host depois do cursor after (até limit eventos; 0 usa o padrão do
// proxy). Host sem proxy de saída responde 501.
func (c *Client) EgressEvents(ctx context.Context, after uint64, limit int) (EgressPage, error) {
	path := fmt.Sprintf("/api/v1/egress/events?after=%d", after)
	if limit > 0 {
		path += fmt.Sprintf("&limit=%d", limit)
	}
	var p EgressPage
	err := c.doJSON(ctx, http.MethodGet, path, nil, &p)
	return p, err
}
