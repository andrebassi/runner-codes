package guestproto

import (
	"context"
	"errors"
	"net"
	"strings"
	"testing"
	"time"

	"runner.code/internal/protocol"
)

// streamGuest responde ao job mandando os eventos dados, na ordem; registra se o job
// chegou com stream ligado.
func streamGuest(gotStream *bool, events ...protocol.Event) func(net.Conn) {
	return func(c net.Conn) {
		var j protocol.Job
		if protocol.ReadFrame(c, &j) != nil {
			return
		}
		*gotStream = j.Stream
		for _, ev := range events {
			protocol.WriteFrame(c, ev)
		}
	}
}

// TestRunStreamDeliversInOrder: os pedaços chegam ao callback na ordem do fio, rotulados
// por fluxo, e o resultado final volta inteiro; o job sai com stream=true.
func TestRunStreamDeliversInOrder(t *testing.T) {
	var gotStream bool
	sock := fakeGuest(t, streamGuest(&gotStream,
		protocol.Event{Type: protocol.EventStdout, Data: "a"},
		protocol.Event{Type: protocol.EventStderr, Data: "e"},
		protocol.Event{Type: protocol.EventStdout, Data: "b"},
		protocol.Event{Type: protocol.EventResult, Result: &protocol.Result{Status: "success", Stdout: "ab", ExitCode: 3}},
	))
	var got []string
	res, err := Client{}.RunStream(context.Background(), sock, protocol.Job{Code: "x"}, func(stream, data string) {
		got = append(got, stream+":"+data)
	})
	if err != nil || res.Stdout != "ab" || res.ExitCode != 3 {
		t.Fatalf("res=%+v err=%v", res, err)
	}
	if !gotStream {
		t.Fatal("the job must go out with stream=true")
	}
	if strings.Join(got, ",") != "stdout:a,stderr:e,stdout:b" {
		t.Fatalf("chunks %v", got)
	}
}

// TestRunStreamBadFrames: evento desconhecido, resultado vazio e conexão fechada antes do
// resultado viram erro, nunca resultado inventado.
func TestRunStreamBadFrames(t *testing.T) {
	var s bool
	cases := map[string]func(net.Conn){
		"unknown event": streamGuest(&s, protocol.Event{Type: "progress"}),
		"empty result":  streamGuest(&s, protocol.Event{Type: protocol.EventResult}),
		"closed early":  streamGuest(&s, protocol.Event{Type: protocol.EventStdout, Data: "a"}),
	}
	for name, h := range cases {
		sock := fakeGuest(t, h)
		if _, err := (Client{}).RunStream(context.Background(), sock, protocol.Job{}, func(string, string) {}); err == nil {
			t.Fatalf("%s: expected an error", name)
		}
	}
	// Imagem com o guest antigo responde um Result solto (frame sem `type`): o erro precisa
	// mandar reconstruir a imagem, não reclamar de "tipo vazio" (medido na 1ª tentativa).
	sock := fakeGuest(t, echoGuest)
	_, err := Client{}.RunStream(context.Background(), sock, protocol.Job{Code: "x"}, func(string, string) {})
	if err == nil || !strings.Contains(err.Error(), "rc image build") {
		t.Fatalf("legacy guest: %v", err)
	}
}

// TestRunStreamDeadline: guest que para de mandar não segura o chamador além do prazo, e
// o erro é o do ctx (o Executor reconhece o timeout por ele).
func TestRunStreamDeadline(t *testing.T) {
	sock := fakeGuest(t, func(c net.Conn) {
		var j protocol.Job
		protocol.ReadFrame(c, &j)
		protocol.WriteFrame(c, protocol.Event{Type: protocol.EventStdout, Data: "a"})
		time.Sleep(2 * time.Second)
	})
	ctx, cancel := context.WithTimeout(context.Background(), 150*time.Millisecond)
	defer cancel()
	start := time.Now()
	_, err := Client{}.RunStream(ctx, sock, protocol.Job{}, func(string, string) {})
	if !errors.Is(err, context.DeadlineExceeded) || time.Since(start) > time.Second {
		t.Fatalf("err=%v after %v", err, time.Since(start))
	}
}

// TestRunStreamConnectError: sem guest no socket, o erro diz que a conexão falhou.
func TestRunStreamConnectError(t *testing.T) {
	_, err := Client{}.RunStream(context.Background(), t.TempDir()+"/none.sock", protocol.Job{}, func(string, string) {})
	if err == nil || !strings.Contains(err.Error(), "connect to guest") {
		t.Fatalf("err=%v", err)
	}
}
