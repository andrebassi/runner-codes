// Package gcedisk guarda o diretório de cada máquina num Persistent Disk do GCE só dela (o PersistentVolume do
// Kubernetes): o disco é criado, anexado e formatado na primeira vez, e montado no diretório da máquina. Quando o host
// some — spot preemptado, servidor apagado —, o disco fica, e o próximo host o adota (anexa e monta) com tudo dentro.
//
// Fala com a API REST do Compute Engine com o token da conta de serviço da própria VM (metadata server), sem SDK: são
// seis chamadas (disco: get/insert/delete; instância: get/attachDisk/detachDisk) e a espera da operação.
//
// Um Persistent Disk zonal gravável tem UM dono: nada aqui toma o disco de um host vivo (ver claim).
package gcedisk

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"

	"runner.codes/pkg/client"
)

// Adapter implementa app.PersistentDisks com Persistent Disk. Os campos Fn são o SO (formatar, montar); nil usa o de
// verdade (mount_linux.go). Project, Zone e Instance vazios são lidos do metadata server na primeira chamada.
type Adapter struct {
	HTTP     *http.Client
	Metadata string // http://metadata.google.internal/computeMetadata/v1
	API      string // https://compute.googleapis.com/compute/v1
	Project  string
	Zone     string
	Instance string
	// DiskType é o tipo do disco novo: pd-balanced (3.000 IOPS de base mesmo pequeno; o pd-standard de 13 GB teria
	// ~10 IOPS e o desktop travaria no boot).
	DiskType string
	// DevDir é onde o udev do GCE cria o link do disco anexado (google-<deviceName>).
	DevDir string
	// FreeTimeout é quanto esperar um dono que está desligando (STOPPING) soltar o disco.
	FreeTimeout time.Duration
	Poll        time.Duration

	FormatFn  func(dev string) error
	MountFn   func(dev, dir string) error
	UnmountFn func(dir string) error
	IsMountFn func(dir string) bool
	// BlankFn diz se o dispositivo nunca foi formatado (o começo dele é todo zero); nil = ler o dispositivo.
	BlankFn func(dev string) (bool, error)

	once    sync.Once
	initErr error
	mu      sync.Mutex
	token   string
	expires time.Time
	locks   sync.Map // nome da máquina -> *sync.Mutex
}

// Erros de dono do disco.
var (
	// ErrBusy é o disco preso a um host que está no ar: tomar dele seria dois hosts gravando o mesmo ext4.
	ErrBusy = errors.New("persistent disk is attached to a running host")
	// ErrStillReleasing é o dono que não soltou o disco no prazo (desligando há mais que FreeTimeout).
	ErrStillReleasing = errors.New("persistent disk is still attached to a host that is shutting down")
)

// diskPrefix marca os discos de máquina no projeto (o Recover acha os seus por ele).
const diskPrefix = client.PersistentDiskPrefix

// invalidLabel é o que não pode entrar num valor de rótulo do GCE.
var invalidLabel = regexp.MustCompile(`[^a-z0-9_-]`)

// DiskName é o nome do disco da máquina (client.PersistentDiskName: a regra mora no SDK porque o orquestrador também
// precisa dela, para apagar o disco de uma máquina que não está em host nenhum).
func DiskName(machine string) string { return client.PersistentDiskName(machine) }

// Mounted diz se dir é o ponto de montagem de um disco.
func (a *Adapter) Mounted(dir string) bool {
	if a.IsMountFn != nil {
		return a.IsMountFn(dir)
	}
	return isMountpoint(dir)
}

// lock devolve a trava da máquina: duas aberturas juntas da mesma máquina não criam dois discos nem montam duas vezes.
func (a *Adapter) lock(name string) func() {
	v, _ := a.locks.LoadOrStore(name, &sync.Mutex{})
	mu := v.(*sync.Mutex)
	mu.Lock()
	return mu.Unlock
}

// Ensure deixa o disco da máquina montado em dir: cria e formata se não existe, adota se existe.
func (a *Adapter) Ensure(ctx context.Context, name, dir string, sizeGiB int) (bool, error) {
	return a.ensure(ctx, name, dir, sizeGiB, "")
}

// EnsureFrom é o Ensure de uma máquina que nasce do snapshot dourado: disco novo é criado A PARTIR do snapshot (o GCE
// entrega o conteúdo sob demanda, em segundos), já com o ext4 e a máquina dentro — nunca é formatado. Disco que já
// existe é adotado como no Ensure, e o snapshot é ignorado: a máquina dele é a que vale.
func (a *Adapter) EnsureFrom(ctx context.Context, name, dir string, sizeGiB int, snapshot string) (bool, error) {
	if snapshot == "" {
		return false, errors.New("ensure from snapshot: no snapshot")
	}
	return a.ensure(ctx, name, dir, sizeGiB, snapshot)
}

// ensure é o Ensure com a origem do disco novo: vazio ("") ou o snapshot dado.
func (a *Adapter) ensure(ctx context.Context, name, dir string, sizeGiB int, snapshot string) (bool, error) {
	defer a.lock(name)()
	if a.Mounted(dir) {
		return false, nil
	}
	if err := a.init(ctx); err != nil {
		return false, err
	}
	disk := DiskName(name)
	d, err := a.getDisk(ctx, disk)
	created := false
	if errors.Is(err, errNotFound) {
		switch ierr := a.insertDisk(ctx, disk, name, sizeGiB, snapshot); {
		case ierr == nil:
			created = true
		case !errors.Is(ierr, errConflict): // 409: outro host criou o mesmo disco agora; segue adotando
			return false, fmt.Errorf("create disk %s: %w", disk, ierr)
		}
		d, err = a.getDisk(ctx, disk)
	}
	if err != nil {
		return false, fmt.Errorf("disk %s: %w", disk, err)
	}
	attached := false
	for _, u := range d.Users {
		owner := path.Base(u)
		if owner == a.Instance {
			attached = true
			continue
		}
		if err := a.claim(ctx, owner, disk); err != nil {
			return false, err
		}
	}
	// Daqui em diante, erro devolve `created`: quem chamou apaga o disco que acabou de nascer. Devolver false fazia
	// o disco vazar sem formatação, e o create seguinte o adotava como pronto (medido em 25/10/2026: o attach falhou
	// por permissão, e a nova tentativa caiu em "mount: invalid argument" num disco cru).
	if !attached {
		if err := a.attach(ctx, disk); err != nil {
			return created, fmt.Errorf("attach %s: %w", disk, err)
		}
	}
	dev := filepath.Join(a.DevDir, "google-"+disk)
	if err := a.waitDevice(ctx, dev); err != nil {
		return created, err
	}
	// Formata pelo CONTEÚDO, não pela flag: um disco adotado que nunca foi formatado (criado por uma tentativa que
	// morreu antes do mkfs) também é formatado. O teste é o começo todo zero, que é como o GCP entrega disco novo;
	// qualquer byte diferente de zero é dado de alguém, e aí se monta — e falha alto se não for ext4, em vez de
	// apagar.
	// Disco que nasceu do snapshot traz o ext4 dele: nunca é "em branco", e nem se lê o começo (a leitura sob demanda
	// do GCE custaria a hidratação do primeiro MiB à toa).
	blank := created && snapshot == ""
	if !created {
		var berr error
		if blank, berr = a.blank(dev); berr != nil {
			return false, fmt.Errorf("read %s: %w", disk, berr)
		}
	}
	if blank {
		if err := a.format(dev); err != nil {
			return created, fmt.Errorf("format %s: %w", disk, err)
		}
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return created, err
	}
	if err := a.mount(dev, dir); err != nil {
		return created, fmt.Errorf("mount %s on %s: %w", disk, dir, err)
	}
	return created, nil
}

// claim tira o disco de outro host, só quando ele não está mais no ar. É a trava contra dois donos (o split-brain):
//
//   - host sumiu (404) ou parou (TERMINATED, STOPPED, SUSPENDED): solta o disco dele;
//   - host desligando (STOPPING — a preempção do spot passa por aqui): espera até FreeTimeout ele parar ou soltar;
//   - host no ar (RUNNING e o resto): recusa com ErrBusy, na hora. Quem chamou tenta de novo depois — o host
//     preemptado solta o disco sozinho (Drain) ou para em ~30 s.
func (a *Adapter) claim(ctx context.Context, owner, disk string) error {
	deadline := time.Now().Add(a.FreeTimeout)
	for {
		status, err := a.instanceStatus(ctx, owner)
		switch {
		case errors.Is(err, errNotFound):
			// Instância apagada: o GCE solta os discos dela sozinho; espera a lista de donos do disco esvaziar.
			if free, ferr := a.freedFrom(ctx, disk, owner); ferr != nil || free {
				return ferr
			}
		case err != nil:
			return fmt.Errorf("owner %s: %w", owner, err)
		case status == "TERMINATED" || status == "STOPPED" || status == "SUSPENDED":
			derr := a.detach(ctx, owner, disk)
			if derr == nil || errors.Is(derr, errNotFound) || errors.Is(derr, errNotAttached) {
				return nil
			}
			// 409: já há uma operação sobre o disco em andamento — o próprio dono soltando na drenagem, ou o GCE
			// soltando ao apagar a instância preemptada. Não é recusa: espera a lista de donos esvaziar e tenta de
			// novo, até o prazo. Medido em 25/10/2026: o detach da drenagem ainda corria quando o host novo pediu o
			// dele, e o desktop ficou 10 min sem voltar porque a primeira resposta era tratada como erro final.
			if !errors.Is(derr, errConflict) {
				return fmt.Errorf("detach %s from %s: %w", disk, owner, derr)
			}
			if free, ferr := a.freedFrom(ctx, disk, owner); ferr != nil || free {
				return ferr
			}
		case status == "STOPPING" || status == "SUSPENDING":
			if free, ferr := a.freedFrom(ctx, disk, owner); ferr != nil || free {
				return ferr
			}
		default:
			return fmt.Errorf("%w (%s is %s)", ErrBusy, owner, status)
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("%w (%s)", ErrStillReleasing, owner)
		}
		if err := a.sleep(ctx); err != nil {
			return err
		}
	}
}

// freedFrom diz se o disco já não está mais preso a owner.
func (a *Adapter) freedFrom(ctx context.Context, disk, owner string) (bool, error) {
	d, err := a.getDisk(ctx, disk)
	if err != nil {
		return false, err
	}
	for _, u := range d.Users {
		if path.Base(u) == owner {
			return false, nil
		}
	}
	return true, nil
}

// Release desmonta e solta o disco deste host.
func (a *Adapter) Release(ctx context.Context, name, dir string) error {
	defer a.lock(name)()
	return a.release(ctx, name, dir)
}

// release é o Release já com a trava da máquina.
func (a *Adapter) release(ctx context.Context, name, dir string) error {
	if err := a.init(ctx); err != nil {
		return err
	}
	if a.Mounted(dir) {
		if err := a.unmountRetry(ctx, dir); err != nil {
			return fmt.Errorf("unmount %s: %w", dir, err)
		}
	}
	if err := a.detach(ctx, a.Instance, DiskName(name)); err != nil && !errors.Is(err, errNotFound) && !errors.Is(err, errNotAttached) {
		return fmt.Errorf("detach %s: %w", DiskName(name), err)
	}
	// O diretório vazio é só o ponto de montagem; sobrando, ele confundiria o próximo create.
	if err := os.Remove(dir); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return nil
}

// Destroy solta e apaga o disco da máquina.
func (a *Adapter) Destroy(ctx context.Context, name, dir string) error {
	defer a.lock(name)()
	if err := a.release(ctx, name, dir); err != nil {
		return err
	}
	if err := a.op(ctx, http.MethodDelete, a.zonal("disks/"+DiskName(name)), nil); err != nil && !errors.Is(err, errNotFound) {
		return fmt.Errorf("delete disk %s: %w", DiskName(name), err)
	}
	return nil
}

// Snapshot tira o snapshot dourado do disco da máquina: solta o disco deste host antes (desmontado e sem anexo, o
// conteúdo está todo gravado — o snapshot de disco anexado e montado só é consistente "como numa queda de energia") e
// espera o GCE terminar. O disco fica, solto: quem publicou decide apagá-lo. Snapshot com o mesmo nome é errConflict
// (os nomes levam a versão, e reescrever o dourado de uma versão em uso trocaria a máquina debaixo de quem a usa).
func (a *Adapter) Snapshot(ctx context.Context, name, dir, snapshot string) error {
	defer a.lock(name)()
	if err := a.release(ctx, name, dir); err != nil {
		return err
	}
	body := map[string]any{"name": snapshot}
	if v := labelValue(name); v != "" {
		body["labels"] = map[string]string{machineLabel: v}
	}
	if err := a.op(ctx, http.MethodPost, a.zonal("disks/"+DiskName(name)+"/createSnapshot"), body); err != nil {
		return fmt.Errorf("snapshot %s: %w", snapshot, err)
	}
	return nil
}

// SnapshotExists diz se o snapshot dourado existe e está pronto para criar disco (status READY).
func (a *Adapter) SnapshotExists(ctx context.Context, snapshot string) (bool, error) {
	if err := a.init(ctx); err != nil {
		return false, err
	}
	var sn struct {
		Status string `json:"status"`
	}
	err := a.do(ctx, http.MethodGet, fmt.Sprintf("%s/projects/%s/global/snapshots/%s", a.API, a.Project, snapshot), nil, &sn)
	if errors.Is(err, errNotFound) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return sn.Status == "READY", nil
}

// Recover remonta os discos de máquina anexados a este host mas não montados (o host reiniciou: o anexo sobrevive
// ao reboot, a montagem não). Devolve quantos remontou.
func (a *Adapter) Recover(ctx context.Context, root string) (int, error) {
	if err := a.init(ctx); err != nil {
		return 0, err
	}
	var inst struct {
		Disks []struct {
			DeviceName string `json:"deviceName"`
		} `json:"disks"`
	}
	if err := a.do(ctx, http.MethodGet, a.zonal("instances/"+a.Instance), nil, &inst); err != nil {
		return 0, err
	}
	n := 0
	for _, d := range inst.Disks {
		if !strings.HasPrefix(d.DeviceName, diskPrefix) {
			continue
		}
		var meta struct {
			Labels map[string]string `json:"labels"`
		}
		if err := a.do(ctx, http.MethodGet, a.zonal("disks/"+d.DeviceName), nil, &meta); err != nil {
			return n, err
		}
		machine := meta.Labels[machineLabel]
		if machine == "" {
			continue // disco sem o rótulo: não se sabe de quem é, melhor não montar
		}
		dir := filepath.Join(root, machine)
		if a.Mounted(dir) {
			continue
		}
		dev := filepath.Join(a.DevDir, "google-"+d.DeviceName)
		if err := a.waitDevice(ctx, dev); err != nil {
			return n, err
		}
		if err := os.MkdirAll(dir, 0o700); err != nil {
			return n, err
		}
		if err := a.mount(dev, dir); err != nil {
			return n, fmt.Errorf("mount %s: %w", d.DeviceName, err)
		}
		n++
	}
	return n, nil
}

// machineLabel guarda o nome original da máquina no disco (o nome do disco é a versão saneada).
const machineLabel = "rc-machine"

// labelValue é o nome da máquina como valor de rótulo do GCE (minúsculas, dígitos, hífen e sublinhado, 63 no
// máximo). Nome que não cabe fica sem rótulo — o Recover não monta disco sem dono conhecido.
func labelValue(machine string) string {
	if len(machine) > 63 || invalidLabel.MatchString(machine) {
		return ""
	}
	return machine
}

// diskInfo é o que importa de um disco do GCE.
type diskInfo struct {
	Users []string `json:"users"`
}

// getDisk lê o disco; ausente é errNotFound.
func (a *Adapter) getDisk(ctx context.Context, name string) (diskInfo, error) {
	var d diskInfo
	err := a.do(ctx, http.MethodGet, a.zonal("disks/"+name), nil, &d)
	return d, err
}

// insertDisk cria o disco (vazio, ou do snapshot dado) e espera a operação.
func (a *Adapter) insertDisk(ctx context.Context, name, machine string, sizeGiB int, snapshot string) error {
	body := map[string]any{
		"name":   name,
		"sizeGb": fmt.Sprint(sizeGiB),
		"type":   fmt.Sprintf("projects/%s/zones/%s/diskTypes/%s", a.Project, a.Zone, a.DiskType),
	}
	if snapshot != "" {
		body["sourceSnapshot"] = fmt.Sprintf("projects/%s/global/snapshots/%s", a.Project, snapshot)
	}
	if v := labelValue(machine); v != "" {
		body["labels"] = map[string]string{machineLabel: v}
	}
	return a.op(ctx, http.MethodPost, a.zonal("disks"), body)
}

// attach anexa o disco a este host, gravável, sem apagar junto com a instância (autoDelete false: o disco é da
// máquina, não do host).
func (a *Adapter) attach(ctx context.Context, name string) error {
	return a.op(ctx, http.MethodPost, a.zonal("instances/"+a.Instance+"/attachDisk"), map[string]any{
		"source":     fmt.Sprintf("projects/%s/zones/%s/disks/%s", a.Project, a.Zone, name),
		"deviceName": name,
		"mode":       "READ_WRITE",
		"autoDelete": false,
	})
}

// detach solta o disco de uma instância.
func (a *Adapter) detach(ctx context.Context, instance, name string) error {
	return a.op(ctx, http.MethodPost, a.zonal("instances/"+instance+"/detachDisk?deviceName="+url.QueryEscape(name)), nil)
}

// instanceStatus é o status de uma instância (RUNNING, STOPPING, TERMINATED…); ausente é errNotFound.
func (a *Adapter) instanceStatus(ctx context.Context, name string) (string, error) {
	var inst struct {
		Status string `json:"status"`
	}
	err := a.do(ctx, http.MethodGet, a.zonal("instances/"+name), nil, &inst)
	return inst.Status, err
}

// operation é uma operação zonal do GCE.
type operation struct {
	Name   string `json:"name"`
	Status string `json:"status"`
	Error  *struct {
		Errors []struct {
			Code    string `json:"code"`
			Message string `json:"message"`
		} `json:"errors"`
	} `json:"error"`
}

// op faz a chamada que devolve uma operação e espera ela terminar (o /wait do GCE segura até 2 min por chamada).
func (a *Adapter) op(ctx context.Context, method, u string, body any) error {
	var o operation
	if err := a.do(ctx, method, u, body, &o); err != nil {
		return err
	}
	for o.Status != "DONE" {
		if err := a.do(ctx, http.MethodPost, a.zonal("operations/"+o.Name+"/wait"), nil, &o); err != nil {
			return err
		}
	}
	if o.Error != nil && len(o.Error.Errors) > 0 {
		e := o.Error.Errors[0]
		// O detach de um disco que não está mais na instância volta como erro da operação, não como 404 — e em duas
		// redações: "is not found in instance" e "No attached disk found with device name" (esta, medida em 25/10/2026
		// na faxina do molde do snapshot dourado: o Snapshot já tinha soltado o disco, e o Destroy seguinte falhava).
		if strings.Contains(e.Message, "is not found in instance") || strings.Contains(e.Message, "No attached disk found") {
			return fmt.Errorf("%w: %s", errNotAttached, e.Message)
		}
		return fmt.Errorf("%s: %s", e.Code, e.Message)
	}
	return nil
}

// Erros de transporte da API.
var (
	errNotFound    = errors.New("not found")
	errConflict    = errors.New("already exists")
	errNotAttached = errors.New("disk not attached")
)

// zonal monta a URL de um recurso da zona.
func (a *Adapter) zonal(rest string) string {
	return fmt.Sprintf("%s/projects/%s/zones/%s/%s", a.API, a.Project, a.Zone, rest)
}

// do faz a chamada autenticada e decodifica o JSON em out (nil ignora). 404 é errNotFound e 409, errConflict.
func (a *Adapter) do(ctx context.Context, method, u string, body, out any) error {
	tok, err := a.accessToken(ctx)
	if err != nil {
		return err
	}
	var rd io.Reader
	if body != nil {
		raw, _ := json.Marshal(body)
		rd = bytes.NewReader(raw)
	}
	req, err := http.NewRequestWithContext(ctx, method, u, rd)
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", "Bearer "+tok)
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	resp, err := a.HTTP.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	raw, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	switch {
	case resp.StatusCode == http.StatusNotFound:
		return errNotFound
	case resp.StatusCode == http.StatusConflict:
		return errConflict
	case resp.StatusCode >= 300:
		return fmt.Errorf("%s %s: %d %s", method, path.Base(strings.SplitN(u, "?", 2)[0]), resp.StatusCode, strings.TrimSpace(string(raw)))
	}
	if out != nil && len(raw) > 0 {
		return json.Unmarshal(raw, out)
	}
	return nil
}

// accessToken devolve o token da conta de serviço da VM, guardado até 1 min antes de vencer.
func (a *Adapter) accessToken(ctx context.Context) (string, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.token != "" && time.Now().Before(a.expires) {
		return a.token, nil
	}
	raw, err := a.metadata(ctx, "instance/service-accounts/default/token")
	if err != nil {
		return "", fmt.Errorf("token: %w", err)
	}
	var t struct {
		AccessToken string `json:"access_token"`
		ExpiresIn   int    `json:"expires_in"`
	}
	if err := json.Unmarshal([]byte(raw), &t); err != nil || t.AccessToken == "" {
		return "", errors.New("token: bad response")
	}
	a.token, a.expires = t.AccessToken, time.Now().Add(time.Duration(t.ExpiresIn)*time.Second-time.Minute)
	return a.token, nil
}

// init lê do metadata server o que não veio preenchido (projeto, zona, nome da instância), uma vez.
func (a *Adapter) init(ctx context.Context) error {
	a.once.Do(func() {
		for _, f := range []struct {
			dst *string
			key string
		}{{&a.Project, "project/project-id"}, {&a.Zone, "instance/zone"}, {&a.Instance, "instance/name"}} {
			if *f.dst != "" {
				continue
			}
			v, err := a.metadata(ctx, f.key)
			if err != nil {
				a.initErr = fmt.Errorf("metadata %s: %w", f.key, err)
				return
			}
			*f.dst = path.Base(strings.TrimSpace(v)) // a zona vem como projects/<n>/zones/<zona>
		}
	})
	return a.initErr
}

// metadata lê uma chave do metadata server.
func (a *Adapter) metadata(ctx context.Context, key string) (string, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, a.Metadata+"/"+key, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("Metadata-Flavor", "Google")
	resp, err := a.HTTP.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	raw, _ := io.ReadAll(io.LimitReader(resp.Body, 64<<10))
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("status %d", resp.StatusCode)
	}
	return string(raw), nil
}

// waitDevice espera o link do disco recém-anexado aparecer (o udev leva de milissegundos a poucos segundos).
func (a *Adapter) waitDevice(ctx context.Context, dev string) error {
	for end := time.Now().Add(30 * time.Second); ; {
		if _, err := os.Stat(dev); err == nil {
			return nil
		}
		if time.Now().After(end) {
			return fmt.Errorf("device %s did not appear", dev)
		}
		if err := a.sleep(ctx); err != nil {
			return err
		}
	}
}

// unmountRetry desmonta, tentando de novo por alguns segundos: logo depois do stop da VM, o processo que segurava
// arquivos do diretório ainda pode estar saindo (EBUSY).
func (a *Adapter) unmountRetry(ctx context.Context, dir string) error {
	var err error
	for i := 0; i < 10; i++ {
		if err = a.unmount(dir); err == nil {
			return nil
		}
		if serr := a.sleep(ctx); serr != nil {
			return serr
		}
	}
	return err
}

// sleep espera um Poll, ou o ctx acabar.
func (a *Adapter) sleep(ctx context.Context) error {
	t := time.NewTimer(a.Poll)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-t.C:
		return nil
	}
}

// blankProbe é quanto do começo do dispositivo o isBlank lê. 1 MiB cobre a tabela de partição e o superbloco do
// ext4 (em 1024) com folga, e é leitura rápida até num disco de rede.
const blankProbe = 1 << 20

// isBlank diz se o primeiro 1 MiB do dispositivo é todo zero: disco que nunca recebeu mkfs. Um dispositivo menor
// que isso conta pelo que tem.
func isBlank(dev string) (bool, error) {
	f, err := os.Open(dev)
	if err != nil {
		return false, err
	}
	defer f.Close()
	buf := make([]byte, blankProbe)
	n, err := io.ReadFull(f, buf)
	if err != nil && !errors.Is(err, io.ErrUnexpectedEOF) && !errors.Is(err, io.EOF) {
		return false, err
	}
	for _, b := range buf[:n] {
		if b != 0 {
			return false, nil
		}
	}
	return true, nil
}

// blank diz se o dispositivo nunca foi formatado (o BlankFn ou a leitura do começo do dispositivo).
func (a *Adapter) blank(dev string) (bool, error) {
	if a.BlankFn != nil {
		return a.BlankFn(dev)
	}
	return isBlank(dev)
}

// format formata o disco novo (o FormatFn ou o mkfs.ext4 do sistema).
func (a *Adapter) format(dev string) error {
	if a.FormatFn != nil {
		return a.FormatFn(dev)
	}
	return mkfsExt4(dev)
}

// mount monta o disco no diretório (o MountFn ou o mount(2) do sistema).
func (a *Adapter) mount(dev, dir string) error {
	if a.MountFn != nil {
		return a.MountFn(dev, dir)
	}
	return mountExt4(dev, dir)
}

// unmount desmonta o diretório (o UnmountFn ou o umount(2) do sistema).
func (a *Adapter) unmount(dir string) error {
	if a.UnmountFn != nil {
		return a.UnmountFn(dir)
	}
	return unmountDir(dir)
}

// New devolve o adapter com os padrões de produção: metadata server, API pública, pd-balanced e 90 s de espera pelo
// dono que desliga (a preempção leva ~30 s; a parada inteira raramente passa de 60).
func New() *Adapter {
	return &Adapter{
		HTTP:        &http.Client{Timeout: 150 * time.Second},
		Metadata:    "http://metadata.google.internal/computeMetadata/v1",
		API:         "https://compute.googleapis.com/compute/v1",
		DiskType:    "pd-balanced",
		DevDir:      "/dev/disk/by-id",
		FreeTimeout: 90 * time.Second,
		Poll:        2 * time.Second,
	}
}
