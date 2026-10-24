package guest

import (
	"context"
	"os"
	"sync"
	"time"

	"runner.codes/internal/objectstore"
	"runner.codes/internal/s3fs"
)

// unmounter é um volume montado que o shutdown desmonta (o servidor FUSE).
type unmounter interface{ Unmount() error }

// mountFUSE monta o núcleo no diretório; trocado nos testes (no host não há /dev/fuse).
var mountFUSE = mountFUSEOS

// s3Stage é onde ficam as cópias de escrita: o tmpfs do guest, que o start.sh monta em /tmp.
var s3Stage = "/tmp"

// s3Mounts guarda os volumes s3:// montados, para o shutdown desmontar antes do sync.
var (
	s3MountsMu sync.Mutex
	s3Mounts   []unmounter
)

// mountS3 monta job.S3 em job.Path. Antes de montar, lista o prefixo uma vez: bucket que não
// existe, credencial errada ou endpoint fora do ar falham aqui, no start da máquina, com o
// motivo — e não como um EIO mudo no primeiro ls do usuário.
func mountS3(job Job) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	if job.S3 == nil || job.S3.Bucket == "" || job.Path == "" {
		return failed(res, "mount-s3: bucket and path are required")
	}
	store, err := objectstore.FromEnv(job.Env, job.S3.Bucket)
	if err != nil {
		return failed(res, "mount-s3: "+err.Error())
	}
	core := s3fs.New(store, job.S3.Prefix, job.S3.ReadOnly, s3Stage)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	if _, err := store.List(ctx, core.Prefix()); err != nil {
		return failed(res, "mount-s3: s3://"+job.S3.Bucket+": "+err.Error())
	}
	if err := os.MkdirAll(job.Path, 0o755); err != nil {
		return failed(res, "mount-s3: "+err.Error())
	}
	srv, err := mountFUSE(job.Path, core)
	if err != nil {
		return failed(res, "mount-s3: "+err.Error())
	}
	s3MountsMu.Lock()
	s3Mounts = append(s3Mounts, srv)
	s3MountsMu.Unlock()
	return res
}

// unmountS3 desmonta os volumes s3:// (o shutdown chama antes do sync). Volume ocupado não
// impede o desligamento: o que estava aberto já subiu no close de cada descritor.
func unmountS3() {
	s3MountsMu.Lock()
	defer s3MountsMu.Unlock()
	for _, m := range s3Mounts {
		m.Unmount()
	}
	s3Mounts = nil
}
