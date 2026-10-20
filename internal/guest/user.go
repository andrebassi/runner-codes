package guest

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"syscall"
)

// userInfo é quem roda um comando (o `user` da máquina ou o --user do exec/shell).
type userInfo struct {
	name     string
	uid, gid uint32
	groups   []uint32
	home     string
}

// Arquivos de contas do guest; trocados nos testes.
var (
	passwdFile = "/etc/passwd"
	groupFile  = "/etc/group"
)

// lookupUser resolve um nome do passwd ou "uid[:gid]" numérico (o jeito de casar com o dono
// de um volume do host). Lê os arquivos na hora: um `adduser` feito no init vale no exec
// seguinte. O os/user do Go sem cgo também lê o passwd, mas não devolve o que falta aqui
// (os grupos suplementares de quem não é o usuário atual).
func lookupUser(spec string) (*userInfo, error) {
	if spec == "" {
		return nil, nil
	}
	entries, err := readColonFile(passwdFile)
	if err != nil && !os.IsNotExist(err) {
		return nil, err
	}
	u := &userInfo{home: "/"}
	if idPart, gidPart, numeric := parseNumeric(spec); numeric {
		u.uid, u.gid, u.name = idPart, idPart, spec
		for _, e := range entries {
			if len(e) >= 6 && e[2] == strconv.FormatUint(uint64(idPart), 10) {
				u.name, u.home = e[0], e[5]
				g, _ := strconv.ParseUint(e[3], 10, 32)
				u.gid = uint32(g)
				break
			}
		}
		if gidPart >= 0 {
			u.gid = uint32(gidPart)
		}
	} else {
		found := false
		for _, e := range entries {
			if len(e) >= 6 && e[0] == spec {
				uid, err1 := strconv.ParseUint(e[2], 10, 32)
				gid, err2 := strconv.ParseUint(e[3], 10, 32)
				if err1 != nil || err2 != nil {
					return nil, fmt.Errorf("user %s: malformed passwd entry", spec)
				}
				u.name, u.uid, u.gid, u.home = spec, uint32(uid), uint32(gid), e[5]
				found = true
				break
			}
		}
		if !found {
			return nil, fmt.Errorf("user %s does not exist in the machine (create it in init, e.g. adduser -D %s)", spec, spec)
		}
	}
	groups, _ := readColonFile(groupFile)
	for _, g := range groups {
		if len(g) < 4 {
			continue
		}
		for _, m := range strings.Split(g[3], ",") {
			if m == u.name {
				if gid, err := strconv.ParseUint(g[2], 10, 32); err == nil && uint32(gid) != u.gid {
					u.groups = append(u.groups, uint32(gid))
				}
			}
		}
	}
	return u, nil
}

// parseNumeric lê "uid" ou "uid:gid"; gid -1 quando não veio.
func parseNumeric(spec string) (uint32, int64, bool) {
	a, b, hasGid := strings.Cut(spec, ":")
	uid, err := strconv.ParseUint(a, 10, 32)
	if err != nil {
		return 0, -1, false
	}
	if !hasGid {
		return uint32(uid), -1, true
	}
	gid, err := strconv.ParseUint(b, 10, 32)
	if err != nil {
		return 0, -1, false
	}
	return uint32(uid), int64(gid), true
}

// readColonFile lê um arquivo no formato do passwd/group, uma linha por entrada.
func readColonFile(path string) ([][]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var out [][]string
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		out = append(out, strings.Split(line, ":"))
	}
	return out, sc.Err()
}

// secretEnv são os segredos da máquina (OpSetEnv): só na memória deste processo, juntos a
// todo comando. O guest de uma máquina vive o quanto ela vive, e o start manda de novo.
var (
	secretMu  sync.Mutex
	secretEnv []string
)

// setSecretEnv troca os segredos guardados (o start manda a lista inteira).
func setSecretEnv(env []string) {
	secretMu.Lock()
	secretEnv = append([]string(nil), env...)
	secretMu.Unlock()
}

// environ é o ambiente de um comando: o do guest, os segredos, o do usuário (HOME, USER,
// LOGNAME) e por último o do pedido, que vence.
func environ(u *userInfo, jobEnv []string) []string {
	env := os.Environ()
	secretMu.Lock()
	env = append(env, secretEnv...)
	secretMu.Unlock()
	if u != nil {
		env = append(env, "HOME="+u.home, "USER="+u.name, "LOGNAME="+u.name)
	}
	return append(env, jobEnv...)
}

// applyUser faz o comando rodar como u (nil: como o guest, root), mantendo o resto do
// SysProcAttr (o Setpgid do workload).
func applyUser(cmd *exec.Cmd, u *userInfo) {
	if u == nil {
		return
	}
	if cmd.SysProcAttr == nil {
		cmd.SysProcAttr = &syscall.SysProcAttr{}
	}
	cmd.SysProcAttr.Credential = &syscall.Credential{Uid: u.uid, Gid: u.gid, Groups: u.groups}
}
