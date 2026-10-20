// Package protocol define a mensagem trocada entre host e guest pelo vsock (porta 5000):
// um prefixo de 4 bytes big-endian com o tamanho, seguido do JSON. Host e guest importam
// este pacote para não manterem duas cópias do formato.
package protocol

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
)

// DefaultPort é a porta vsock em que o guest-runner escuta dentro da VM.
const DefaultPort = 5000

// MaxFrameBytes limita o tamanho declarado no prefixo. Sem teto, um prefixo hostil
// (até 4 GiB) faria o leitor alocar tudo antes de ler um byte do corpo. 64 MiB cobre
// stdout grande com folga; acima disso a execução já seria recusada pelo host.
const MaxFrameBytes = 64 << 20

// File é um arquivo-fonte de um job com vários arquivos.
type File struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

// Job é o pedido de execução enviado ao guest.
type Job struct {
	TraceID string            `json:"trace_id"`
	Lang    string            `json:"lang"`
	Code    string            `json:"code"`
	Files   []File            `json:"files,omitempty"`
	Stdin   interface{}       `json:"stdin,omitempty"` // string, ou []string para lote
	Timeout int               `json:"timeout"`
	Meta    map[string]string `json:"meta,omitempty"`
	// Stream pede a saída em pedaços (frames Event) antes do resultado. Sem ele o guest
	// responde com um único frame Result, como sempre fez.
	Stream bool `json:"stream,omitempty"`

	// Op escolhe a operação; vazio é rodar código de uma linguagem (o job de sempre).
	// As outras servem às máquinas persistentes.
	Op string `json:"op,omitempty"`
	// Argv, Env e Cwd são do OpExec: o programa roda sem shell implícito.
	Argv []string `json:"argv,omitempty"`
	Env  []string `json:"env,omitempty"`
	Cwd  string   `json:"cwd,omitempty"`
	// Path, Offset, Data, Final e Mode são do OpPut/OpGet (cópia de arquivo em pedaços).
	Path   string `json:"path,omitempty"`
	Offset int64  `json:"offset,omitempty"`
	Data   []byte `json:"data,omitempty"`
	Final  bool   `json:"final,omitempty"`
	Mode   uint32 `json:"mode,omitempty"`
	// Rows e Cols são o tamanho inicial do terminal no OpPTY.
	Rows uint16 `json:"rows,omitempty"`
	Cols uint16 `json:"cols,omitempty"`
	// Signal e StopOnExit são do workload: o OpWorkload liga o StopOnExit (a máquina
	// desliga quando o workload sai); o OpShutdown manda Signal ao workload e espera Timeout
	// segundos antes do SIGKILL.
	Signal     int  `json:"signal,omitempty"`
	StopOnExit bool `json:"stop_on_exit,omitempty"`
	// Port é do OpDial: a porta TCP que o guest disca no próprio 127.0.0.1.
	Port uint16 `json:"port,omitempty"`
	// User é quem roda o comando (exec, pty, workload): nome do passwd do guest ou
	// "uid[:gid]". Vazio é root, o padrão do guest.
	User string `json:"user,omitempty"`
	// S3 é do OpMountS3: o bucket montado em Path; as credenciais vão em Env (as AWS_* da
	// máquina), nunca no host.
	S3 *S3Mount `json:"s3,omitempty"`
	// Root, TTY, UID, GID e Groups são do OpProc (processo de container de um pod): o programa
	// roda em chroot no Root, com o usuário numérico da spec OCI e só o Env dela.
	Root   string   `json:"root,omitempty"`
	TTY    bool     `json:"tty,omitempty"`
	UID    uint32   `json:"uid,omitempty"`
	GID    uint32   `json:"gid,omitempty"`
	Groups []uint32 `json:"groups,omitempty"`
}

// S3Mount é um volume s3://BUCKET[/PREFIX] montado por FUSE dentro do guest.
type S3Mount struct {
	Bucket   string `json:"bucket"`
	Prefix   string `json:"prefix,omitempty"`
	ReadOnly bool   `json:"read_only,omitempty"`
}

// Operações de Job. OpRun é o vazio, para o fio dos jobs efêmeros não mudar.
const (
	OpRun      = ""
	OpExec     = "exec"
	OpPut      = "put"
	OpGet      = "get"
	OpShutdown = "shutdown"
	// OpPTY abre uma sessão interativa: o guest roda o Argv num PTY e a conexão vira um
	// canal de frames Event nos dois sentidos até o programa sair.
	OpPTY = "pty"
	// OpWorkload liga o processo que a máquina mantém rodando (entrypoint + cmd do
	// runner.code), em segundo plano; a resposta vem assim que ele começou.
	OpWorkload = "workload"
	// OpDial liga a conexão a 127.0.0.1:Port dentro do guest: depois do Result de sucesso a
	// conexão vira um tubo de bytes cru. É por onde chega a porta publicada, para alcançar
	// também servidor que escuta só em localhost (vite, openclaw, dev servers).
	OpDial = "dial"
	// OpMountS3 monta o bucket de S3 em Path (FUSE no guest) e responde depois de conferir que
	// o bucket responde com as credenciais dadas: bucket errado falha o start, não o 1º ls.
	OpMountS3 = "mount-s3"
	// OpSetEnv guarda Env na memória do guest e junta a todo comando seguinte (exec, pty,
	// workload): os segredos da máquina, mandados a cada start e nunca gravados em disco.
	OpSetEnv = "set-env"
	// OpSSHAgent abre no guest o socket Unix Path, que repassa cada conexão à porta vsock Port
	// do host (CID 2) — o agente SSH de quem deu o start.
	OpSSHAgent = "ssh-agent"
	// OpProc roda um processo de container (Kubernetes): chroot no Root, stdin/stdout/stderr
	// ou um TTY pela conexão, sinais vindos do host. A conexão vive até o processo sair.
	OpProc = "proc"
)

// SSHAgentPort é a porta vsock do encaminhamento do agente SSH (a 5000 é a do guest).
const SSHAgentPort = 5001

// MountSocketPortBase é a primeira porta vsock dos sockets do host montados no guest
// (--mount-socket): o i-ésimo usa MountSocketPortBase+i. Longe da 5000 (guest) e da 5001
// (agente SSH), com espaço para os 64 que a máquina pode ter.
const MountSocketPortBase = 5100

// GuestSSHAgentSocket é onde o guest expõe o agente: em /tmp (tmpfs), que some no stop.
const GuestSSHAgentSocket = "/tmp/rc-ssh-agent.sock"

// Tipos de Event da sessão interativa. Do host para o guest: entrada e tamanho da janela.
// Do guest para o host: a saída do terminal e a saída do programa (com o código).
const (
	EventInput  = "input"
	EventResize = "resize"
	EventOutput = "output"
	EventExit   = "exit"
	// EventEOF fecha o stdin do processo (CloseIO do containerd); EventSignal manda o sinal
	// Code ao processo (Kill). Os dois só no OpProc.
	EventEOF    = "eof"
	EventSignal = "signal"
)

// Variáveis de ambiente com que o host configura a rede do guest (krun_set_exec): o guest
// lê no boot e monta eth0, rota e resolv.conf. Ficam aqui porque os dois lados as usam.
const (
	EnvNetAddr    = "RUNNER_NET_ADDR"    // IP/prefixo do guest, ex. 100.96.0.2/30
	EnvNetGateway = "RUNNER_NET_GATEWAY" // rota padrão
	EnvNetDNS     = "RUNNER_NET_DNS"     // resolvedores, separados por vírgula
)

// ChunkBytes é o pedaço de cópia de arquivo: 1 MiB. Em base64 no JSON vira
// ~1,4 MiB, longe do teto do frame, e o arquivo grande não precisa caber na memória.
const ChunkBytes = 1 << 20

// MaxCopyBytes é o teto de um arquivo copiado (4 GiB): acima disso, montar
// diretório é o caminho, não copiar.
const MaxCopyBytes = 4 << 30

// Tipos de Event no modo streaming: pedaços de saída e o resultado que encerra a conversa.
const (
	EventStdout = "stdout"
	EventStderr = "stderr"
	EventResult = "result"
)

// Event é um frame do modo streaming. Pedaço de saída leva Data; o último frame tem
// Type EventResult e o Result completo (stdout/stderr inteiros, para o contrato não mudar).
type Event struct {
	Type   string  `json:"type"`
	Data   string  `json:"data,omitempty"`
	Result *Result `json:"result,omitempty"`
	// Bytes, Rows, Cols e Code são da sessão interativa (OpPTY). Bytes e não Data porque o
	// terminal carrega byte cru (UTF-8 partido no meio, sequências de controle): numa string
	// JSON o byte inválido viraria U+FFFD; em []byte vai em base64, sem perda.
	Bytes []byte `json:"bytes,omitempty"`
	Rows  uint16 `json:"rows,omitempty"`
	Cols  uint16 `json:"cols,omitempty"`
	Code  int    `json:"code,omitempty"` // ausente = 0, que é o que o exit sem código quer dizer
}

// Result é a resposta do guest. Stderr e Exception são null quando vazios, contrato da API.
type Result struct {
	TraceID       string      `json:"trace_id,omitempty"`
	Status        string      `json:"status"`
	Stdout        string      `json:"stdout"`
	Stderr        interface{} `json:"stderr"`
	Exception     interface{} `json:"exception"`
	ExecutionTime int64       `json:"executionTime"`
	ExitCode      int         `json:"exit_code,omitempty"`
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"`
	// Data e Size são do OpGet: o pedaço lido a partir do Offset e o tamanho total do
	// arquivo, para o host saber quando parar.
	Data []byte `json:"data,omitempty"`
	Size int64  `json:"size,omitempty"`
}

// WriteFrame serializa v em JSON e escreve prefixo e corpo numa única chamada,
// para o guest nunca ler um prefixo sem o corpo correspondente.
func WriteFrame(w io.Writer, v interface{}) error {
	body, err := json.Marshal(v)
	if err != nil {
		return fmt.Errorf("marshal frame: %w", err)
	}
	if len(body) > MaxFrameBytes {
		return fmt.Errorf("frame of %d bytes exceeds the limit of %d", len(body), MaxFrameBytes)
	}
	buf := make([]byte, 4+len(body))
	binary.BigEndian.PutUint32(buf, uint32(len(body)))
	copy(buf[4:], body)
	_, err = w.Write(buf)
	return err
}

// ReadFrame lê um frame e decodifica o JSON em v, recusando tamanho acima de MaxFrameBytes.
func ReadFrame(r io.Reader, v interface{}) error {
	var hdr [4]byte
	if _, err := io.ReadFull(r, hdr[:]); err != nil {
		return fmt.Errorf("read frame length: %w", err)
	}
	n := binary.BigEndian.Uint32(hdr[:])
	if n > MaxFrameBytes {
		return fmt.Errorf("frame of %d bytes exceeds the limit of %d", n, MaxFrameBytes)
	}
	body := make([]byte, n)
	if _, err := io.ReadFull(r, body); err != nil {
		return fmt.Errorf("read frame body: %w", err)
	}
	if err := json.Unmarshal(body, v); err != nil {
		return fmt.Errorf("decode frame: %w", err)
	}
	return nil
}
