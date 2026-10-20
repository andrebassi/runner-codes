package k8sshim

import (
	"os"
	"path/filepath"

	"github.com/containerd/containerd/api/types"
	"github.com/containerd/containerd/v2/core/mount"
)

// MountRootfs é o RootfsMounter de verdade: monta as camadas que o containerd mandou (overlay do
// snapshotter) em <bundle>/rootfs, como o shim do runc faz. Sem montagens (rootfs já pronto, ctr com
// --rootfs), usa o root da spec, relativo ao bundle.
func MountRootfs(bundle string, mounts []*types.Mount, specRoot string) (string, func() error, error) {
	if len(mounts) == 0 {
		dir := specRoot
		if !filepath.IsAbs(dir) {
			dir = filepath.Join(bundle, dir)
		}
		return dir, func() error { return nil }, nil
	}
	dir := filepath.Join(bundle, "rootfs")
	if err := os.MkdirAll(dir, 0o711); err != nil {
		return "", nil, err
	}
	var ms []mount.Mount
	for _, m := range mounts {
		ms = append(ms, mount.Mount{Type: m.Type, Source: m.Source, Target: m.Target, Options: m.Options})
	}
	if err := mount.All(ms, dir); err != nil {
		return "", nil, err
	}
	return dir, func() error { return mount.UnmountAll(dir, 0) }, nil
}
