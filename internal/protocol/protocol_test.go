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
		t.Fatalf("prefix %d does not match body %d", got, buf.Len()-4)
	}
	var out Job
	if err := ReadFrame(&buf, &out); err != nil {
		t.Fatal(err)
	}
	if out.TraceID != in.TraceID || out.Code != in.Code || len(out.Files) != 1 || out.Files[0].Name != "a.py" {
		t.Fatalf("frame read differs from frame written: %+v", out)
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
		t.Fatalf("stderr/exception should be null: %s", body)
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
		"no prefix":      {0x00, 0x01},
		"hostile prefix": huge,
		"truncated body": append(short, []byte("abc")...),
		"invalid json":   badJSON,
	}
	for name, raw := range cases {
		t.Run(name, func(t *testing.T) {
			var r Result
			if err := ReadFrame(bytes.NewReader(raw), &r); err == nil {
				t.Fatal("expected an error")
			}
		})
	}
}

// failWriter simula conexão quebrada no meio da escrita.
type failWriter struct{}

// Write sempre falha.
func (failWriter) Write([]byte) (int, error) { return 0, errors.New("connection closed") }

// TestWriteFrameErrors cobre valor não serializável, frame grande demais e escrita que falha.
func TestWriteFrameErrors(t *testing.T) {
	if err := WriteFrame(&bytes.Buffer{}, make(chan int)); err == nil {
		t.Fatal("channel is not serializable; expected an error")
	}
	big := Result{Stdout: strings.Repeat("a", MaxFrameBytes)}
	if err := WriteFrame(&bytes.Buffer{}, big); err == nil {
		t.Fatal("expected rejection of a frame above the limit")
	}
	if err := WriteFrame(failWriter{}, Job{}); err == nil {
		t.Fatal("expected the writer error")
	}
}

// TestStreamWireFormat trava o fio do streaming: `stream` só aparece quando ligado (o job
// sem streaming sai byte a byte igual ao de antes) e o Event carrega o Result final.
func TestStreamWireFormat(t *testing.T) {
	var buf bytes.Buffer
	if err := WriteFrame(&buf, Job{Lang: "sh", Code: "true"}); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(buf.String(), "stream") {
		t.Fatalf("non-streaming job must not carry the field: %s", buf.String()[4:])
	}
	buf.Reset()
	if err := WriteFrame(&buf, Event{Type: EventResult, Result: &Result{Status: "success", Stdout: "a"}}); err != nil {
		t.Fatal(err)
	}
	var ev Event
	if err := ReadFrame(&buf, &ev); err != nil {
		t.Fatal(err)
	}
	if ev.Type != EventResult || ev.Result == nil || ev.Result.Stdout != "a" || ev.Data != "" {
		t.Fatalf("result event read wrong: %+v", ev)
	}
}
