package domain

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"regexp"
)

// visitorRe aceita o visitorId do FingerprintJS (alfanumérico, 20 caracteres na versão aberta) com folga: 8 a 64
// letras e números. Qualquer outra coisa (barra, ponto, espaço) é recusada antes de virar nome de máquina.
var visitorRe = regexp.MustCompile(`^[A-Za-z0-9]{8,64}$`)

// VisitorPrefix é o começo do nome de toda máquina de visitante do playground (e só delas: o reaper só mexe nestas).
const VisitorPrefix = "pg-"

// VisitorMachineName transforma o visitorId do navegador no nome da máquina daquele visitante: pg- e os 10 primeiros
// hexa do sha256. O hash evita que o fingerprint cru apareça em nome de máquina, log e subdomínio de porta, e 40 bits
// bastam para não colidir entre os visitantes de um playground (dezenas, não milhões).
func VisitorMachineName(visitor string) (string, error) {
	if !visitorRe.MatchString(visitor) {
		return "", ValidationError{Err: errors.New("visitor id must be 8 to 64 letters or digits")}
	}
	sum := sha256.Sum256([]byte(visitor))
	return VisitorPrefix + hex.EncodeToString(sum[:])[:10], nil
}
