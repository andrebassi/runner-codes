package protocol

import (
	"bytes"
	"encoding/binary"
	"errors"
	"strings"
	"testing"
)

// TestRoundTrip trava o formato: o que WriteFrame escreve, ReadFrame lê igual.
func TestRoundTrip(t *testing.T) {
	var buf bytes.Buffer
	in := Job{TraceID: "t1", Lang: "python", Code: "print(1)", Timeout: 5, Files: []File{{Name: "a.py", Content: "x"}}}
	if err := WriteFrame(&buf, in); err != nil {
		t.Fatal(err)
	}
	if got := binary.BigEndian.Uint32(buf.Bytes()[:4]); int(got) != buf.Len()-4 {
		t.Fatalf("prefixo %d não bate com o corpo %d", got, buf.Len()-4)
	}
	var out Job
	if err := ReadFrame(&buf, &out); err != nil {
		t.Fatal(err)
	}
	if out.TraceID != in.TraceID || out.Code != in.Code || len(out.Files) != 1 || out.Files[0].Name != "a.py" {
		t.Fatalf("frame lido diferente do escrito: %+v", out)
	}
}

// TestNullFieldsStayNull trava o contrato da API: stderr e exception vazios saem como null.
func TestNullFieldsStayNull(t *testing.T) {
	var buf bytes.Buffer
	if err := WriteFrame(&buf, Result{Status: "success", Stdout: "ok"}); err != nil {
		t.Fatal(err)
	}
	body := buf.String()[4:]
	if !strings.Contains(body, `"stderr":null`) || !strings.Contains(body, `"exception":null`) {
		t.Fatalf("stderr/exception deveriam ser null: %s", body)
	}
}

// TestReadFrameErrors cobre cada ramo de falha do leitor, inclusive o prefixo hostil.
func TestReadFrameErrors(t *testing.T) {
	huge := make([]byte, 4)
	binary.BigEndian.PutUint32(huge, MaxFrameBytes+1)
	short := make([]byte, 4)
	binary.BigEndian.PutUint32(short, 10)
	badJSON := append(make([]byte, 4), []byte("{x")...)
	binary.BigEndian.PutUint32(badJSON, 2)
	cases := map[string][]byte{
		"sem prefixo":      {0x00, 0x01},
		"prefixo hostil":   huge,
		"corpo incompleto": append(short, []byte("abc")...),
		"json inválido":    badJSON,
	}
	for name, raw := range cases {
		t.Run(name, func(t *testing.T) {
			var r Result
			if err := ReadFrame(bytes.NewReader(raw), &r); err == nil {
				t.Fatal("esperava erro")
			}
		})
	}
}

// failWriter simula conexão quebrada no meio da escrita.
type failWriter struct{}

// Write sempre falha.
func (failWriter) Write([]byte) (int, error) { return 0, errors.New("conexão fechada") }

// TestWriteFrameErrors cobre valor não serializável, frame grande demais e escrita que falha.
func TestWriteFrameErrors(t *testing.T) {
	if err := WriteFrame(&bytes.Buffer{}, make(chan int)); err == nil {
		t.Fatal("canal não é serializável; esperava erro")
	}
	big := Result{Stdout: strings.Repeat("a", MaxFrameBytes)}
	if err := WriteFrame(&bytes.Buffer{}, big); err == nil {
		t.Fatal("esperava recusa de frame acima do limite")
	}
	if err := WriteFrame(failWriter{}, Job{}); err == nil {
		t.Fatal("esperava o erro do writer")
	}
}
