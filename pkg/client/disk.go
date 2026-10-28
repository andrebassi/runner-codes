package client

import (
	"crypto/sha256"
	"encoding/hex"
	"regexp"
	"strings"
)

// PersistentDiskPrefix marca, no projeto do GCE, os discos de máquina do rc (rc serve --persistent-disks gce).
const PersistentDiskPrefix = "rc-pd-"

// invalidDiskName é o que não pode entrar num nome de recurso do GCE ([a-z]([-a-z0-9]*[a-z0-9])?).
var invalidDiskName = regexp.MustCompile(`[^a-z0-9-]+`)

// PersistentDiskName é o nome do Persistent Disk da máquina: o prefixo mais o nome dela. Nome que o GCE não aceita
// como está (maiúscula, sublinhado, longo demais) é saneado e ganha 8 hex do sha256 do original no fim — dois nomes
// que viram a mesma versão saneada não caem no mesmo disco. Mora no SDK porque o orquestrador também precisa dela: é
// ele quem apaga o disco de uma máquina que não está em host nenhum.
func PersistentDiskName(machine string) string {
	n := strings.Trim(invalidDiskName.ReplaceAllString(strings.ToLower(machine), "-"), "-")
	out := PersistentDiskPrefix + n
	if len(out) <= 63 && n == machine {
		return out
	}
	sum := sha256.Sum256([]byte(machine))
	keep := min(len(out), 63-9)
	return strings.TrimRight(out[:keep], "-") + "-" + hex.EncodeToString(sum[:4])
}
