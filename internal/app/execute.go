package app

import (
	"context"
	"errors"
	"fmt"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// timeoutGrace é a folga do host sobre o timeout do job. O executor do guest mata o
// programa no timeout e responde; o deadline do host só age se o guest não responder
// (VM travada), e aí a VM é morta de qualquer forma.
const timeoutGrace = 2 * time.Second

// guestTimeoutException é o texto que o executor do guest põe em exception quando o
// programa passa do timeout (internal/guest/executor.go).
const guestTimeoutException = "Timeout"

// VMSource entrega e destrói VMs; o Pool implementa.
type VMSource interface {
	Take(ctx context.Context, lang string) (*ReadyVM, error)
	Destroy(r *ReadyVM)
}

// Executor roda um job numa VM que nunca rodou outro job.
type Executor struct {
	VMs     VMSource
	Guest   ports.GuestClient
	Images  ports.ImageStore
	Catalog Catalog
}

// Known diz se a linguagem está no catálogo e tem imagem construída neste host.
func (e *Executor) Known(lang string) bool {
	if _, ok := e.Catalog.Get(lang); !ok {
		return false
	}
	_, ok := e.Images.ImagePath(lang)
	return ok
}

// Run valida, pega uma VM, executa e destrói a VM em qualquer desfecho.
func (e *Executor) Run(ctx context.Context, job domain.Job) (domain.Result, error) {
	job = job.Normalize()
	if err := job.Validate(e.Known); err != nil {
		return domain.Result{TraceID: job.TraceID}, err
	}
	vm, err := e.VMs.Take(ctx, job.Language)
	if err != nil {
		return domain.Result{TraceID: job.TraceID}, fmt.Errorf("get VM: %w", err)
	}
	defer e.VMs.Destroy(vm)

	runCtx, cancel := context.WithTimeout(ctx, job.Timeout+timeoutGrace)
	defer cancel()
	start := time.Now()
	res, err := e.Guest.Run(runCtx, vm.Socket, toProtocol(job))
	elapsed := time.Since(start)
	if errors.Is(err, context.DeadlineExceeded) && ctx.Err() == nil {
		return domain.Result{TraceID: job.TraceID, Status: "failed", TimedOut: true, Duration: elapsed, Error: domain.ErrTimeout.Error()}, nil
	}
	if err != nil {
		return domain.Result{TraceID: job.TraceID}, fmt.Errorf("run in guest: %w", err)
	}
	return fromProtocol(job.TraceID, res, elapsed), nil
}

// toProtocol converte o job do domínio para o formato do fio.
func toProtocol(j domain.Job) protocol.Job {
	pj := protocol.Job{TraceID: j.TraceID, Lang: j.Language, Code: j.Code, Timeout: int(j.Timeout / time.Second)}
	if j.Stdin != "" {
		pj.Stdin = j.Stdin
	}
	for _, f := range j.Files {
		pj.Files = append(pj.Files, protocol.File{Name: f.Name, Content: f.Content})
	}
	return pj
}

// fromProtocol converte a resposta do guest. Stderr e Exception chegam como interface{}
// (null ou string) por causa do contrato da API.
func fromProtocol(traceID string, r protocol.Result, elapsed time.Duration) domain.Result {
	out := domain.Result{
		TraceID: traceID, Status: r.Status, Stdout: r.Stdout, ExitCode: r.ExitCode,
		Duration: elapsed, Error: r.Error,
		Stderr: asString(r.Stderr), Exception: asString(r.Exception),
	}
	if r.ExecutionTime > 0 {
		out.Duration = time.Duration(r.ExecutionTime) * time.Millisecond
	}
	// O executor do guest sinaliza timeout com exception "Timeout" (status continua
	// "success": no contrato herdado, status diz que a API executou, não que o programa
	// passou). E001 no campo error também vale, para o caminho de erro de operação.
	if out.Exception == guestTimeoutException || out.Error == domain.ErrTimeout.Error() {
		out.TimedOut = true
	}
	return out
}

// asString transforma null em "" e qualquer outro valor em texto.
func asString(v interface{}) string {
	switch t := v.(type) {
	case nil:
		return ""
	case string:
		return t
	default:
		return fmt.Sprint(t)
	}
}
