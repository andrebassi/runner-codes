package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"runner.code/internal/app"
	"runner.code/internal/domain"
	"runner.code/internal/pack"
)

// packRef é a referência de imagem de uma máquina que veio de um pack: o BuildImage a
// reconhece e não tenta baixar nada (a imagem já está extraída no cache).
const packRef = "pack:"

// packsDir é onde os payloads ficam extraídos, um diretório por pack.
func packsDir() string { return filepath.Join(cacheDir(), "packs") }

// openPayload abre o payload de path: o próprio arquivo .rcmachine, ou um executável que
// carrega o payload (sidecar ao lado ou anexado).
func openPayload(path string) (*os.File, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	magic := make([]byte, len(pack.Magic))
	if _, err := io.ReadFull(f, magic); err == nil && string(magic) == pack.Magic {
		f.Seek(0, io.SeekStart)
		return f, nil
	}
	f.Close()
	pf, ok, err := pack.Locate(path)
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, fmt.Errorf("%s is not a runner.code pack", path)
	}
	return pf, nil
}

// imported é um pack extraído no cache: a máquina base e o disco de partida (se veio de uma
// máquina).
type imported struct {
	manifest pack.Manifest
	machine  domain.Machine
	disk     string
}

// importPack extrai o payload de path (uma vez: o diretório é pela impressão do manifesto) e
// registra a imagem dele com o nome que o create procura.
func importPack(path string) (imported, error) {
	var out imported
	f, err := openPayload(path)
	if err != nil {
		return out, err
	}
	defer f.Close()
	start, _ := f.Seek(0, io.SeekCurrent)
	m, _, err := pack.ReadManifest(f)
	if err != nil {
		return out, err
	}
	key := pack.Key(m)
	dir := filepath.Join(packsDir(), key)
	if _, err := os.Stat(filepath.Join(dir, ".ready")); err != nil {
		f.Seek(start, io.SeekStart)
		tmp := dir + ".tmp"
		os.RemoveAll(tmp)
		if _, err := pack.Extract(f, tmp); err != nil {
			os.RemoveAll(tmp)
			return out, err
		}
		os.WriteFile(filepath.Join(tmp, ".ready"), nil, 0o644)
		os.RemoveAll(dir)
		if err := os.Rename(tmp, dir); err != nil {
			return out, err
		}
	}
	pack.Touch(dir)
	ref := packRef + key
	images := filepath.Join(cacheDir(), "images")
	os.MkdirAll(images, 0o755)
	link := filepath.Join(images, app.ImageStackName(ref)+".ext4")
	os.Remove(link)
	// Relativo: o ImagePath junta o alvo ao diretório das imagens.
	if err := os.Symlink(filepath.Join("..", "packs", key, "root.ext4"), link); err != nil {
		return out, err
	}
	out.manifest, out.machine = m, m.Machine
	out.machine.Stack, out.machine.Image = "", ref
	if _, err := os.Stat(filepath.Join(dir, "data.ext4")); err == nil {
		out.disk = filepath.Join(dir, "data.ext4")
	}
	return out, nil
}

// packBuildImage é o BuildImage de uma máquina de pack: a imagem já está no cache (o import
// a registrou); se sumiu, o erro diz como voltar, em vez de tentar baixar "pack:...".
func packBuildImage(d *deps, l domain.Language) error {
	if _, ok := d.builder.ImagePath(l.Name); ok {
		return nil
	}
	return fmt.Errorf("image of %s is not in the cache: run the pack again (or rc machine create --from it)", l.Image)
}

// packCmd agrupa os subcomandos de pack.
func packCmd() *cobra.Command {
	c := &cobra.Command{Use: "pack", Short: "Portable artifacts: a machine or image in a file that runs on another host"}
	c.AddCommand(packCreateCmd(), packInspectCmd(), packPruneCmd())
	return c
}

// packCreateCmd grava o artefato: o stub (este rc) e o payload ao lado, ou num arquivo só.
func packCreateCmd() *cobra.Command {
	var mf *machineFlags
	var output, fromMachine string
	var single bool
	c := &cobra.Command{
		Use:   "create (--stack S | --image REF | --from-vm NAME) --output PATH [-- workload...]",
		Short: "Pack an image or a stopped machine into a portable artifact",
		Example: `  rc pack create --stack python --output ./py312 && ./py312 run -- python3 --version
  rc pack create --from-vm dev --output ./dev --single-file`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if output == "" {
				return errors.New("--output is required")
			}
			d, err := wire()
			if err != nil {
				return err
			}
			m := d.newMachines()
			ctx := cmd.Context()
			var mc domain.Machine
			var files []string
			source := ""
			if fromMachine != "" {
				st, err := m.Status(fromMachine)
				if err != nil {
					return err
				}
				if st.State != app.StateStopped {
					return fmt.Errorf("machine %s is %s: stop it first (a running disk is not consistent)", fromMachine, st.State)
				}
				mc, source = st.Machine, "machine:"+fromMachine
				root, ok := d.builder.ImagePath(mc.Stack)
				if !ok {
					return fmt.Errorf("image of machine %s (%s) is not in the cache", fromMachine, mc.Stack)
				}
				files = []string{linkedAs(root, "root.ext4"), linkedAs(filepath.Join(cacheDir(), "machines", fromMachine, "disk.ext4"), "data.ext4")}
			} else {
				if mc, _, err = mf.build(cmd.Flags(), args); err != nil {
					return err
				}
				root, err := packRoot(ctx, d, &mc)
				if err != nil {
					return err
				}
				source = "stack:" + mc.Stack
				if mc.Image != "" {
					source = "image:" + mc.Image
				}
				files = []string{linkedAs(root, "root.ext4")}
			}
			defer func() {
				for _, f := range files {
					os.RemoveAll(filepath.Dir(f)) // o diretório temporário do link
				}
			}()
			if mc, err = pack.Portable(mc); err != nil {
				return err
			}
			mc.Name = filepath.Base(output)
			if err := domain.ValidateName(mc.Name); err != nil {
				return fmt.Errorf("--output: the file name becomes the machine name: %w", err)
			}
			return writeArtifact(output, single, pack.Manifest{Name: mc.Name, Created: time.Now().UTC().Truncate(time.Second),
				RCVersion: Version, Platform: "linux/" + runtime.GOARCH, HostPlatform: runtime.GOOS + "/" + runtime.GOARCH,
				Source: source, Machine: mc}, files)
		},
	}
	mf = addMachineFlags(c.Flags(), "pack")
	c.Flags().StringVarP(&output, "output", "o", "", "artifact path (the stub; the payload goes next to it as PATH"+pack.SidecarExt+")")
	c.Flags().StringVar(&fromMachine, "from-vm", "", "pack a stopped machine, with what was installed and written in it")
	c.Flags().BoolVar(&single, "single-file", false, "one executable with the payload appended, instead of stub + sidecar")
	return c
}

// packRoot devolve a imagem da máquina declarada, construindo se ainda não existe.
func packRoot(ctx context.Context, d *deps, mc *domain.Machine) (string, error) {
	stack := mc.Stack
	if mc.Image != "" {
		stack = app.ImageStackName(mc.Image)
		if err := buildImage(ctx, d, domain.Language{Name: stack, Image: mc.Image, CPUs: mc.CPUs, MemoryMiB: mc.MemoryMiB}); err != nil {
			return "", err
		}
	} else if _, ok := d.builder.ImagePath(stack); !ok {
		l, ok := d.catalog.Get(stack)
		if !ok {
			return "", domain.ErrUnsupportedLanguage{Name: stack}
		}
		if _, err := d.builder.Build(ctx, l); err != nil {
			return "", err
		}
	}
	root, ok := d.builder.ImagePath(stack)
	if !ok {
		return "", fmt.Errorf("image of %s is not in the cache", stack)
	}
	return root, nil
}

// linkedAs dá ao arquivo o nome que ele terá no payload (o pack usa o basename), por um
// link num diretório temporário; o arquivo original não é copiado.
func linkedAs(path, name string) string {
	dir, _ := os.MkdirTemp("", "rcpack")
	link := filepath.Join(dir, name)
	os.Symlink(path, link)
	return link
}

// writeArtifact copia este rc como stub e grava o payload (sidecar ou anexado).
func writeArtifact(output string, single bool, m pack.Manifest, files []string) error {
	exe, err := os.Executable()
	if err != nil {
		return err
	}
	if err := copyFile(exe, output, 0o755); err != nil {
		return fmt.Errorf("stub: %w", err)
	}
	var payload bytes.Buffer
	target := output + pack.SidecarExt
	if !single {
		f, err := os.Create(target)
		if err != nil {
			return err
		}
		if err := pack.Write(f, m, files); err != nil {
			f.Close()
			return err
		}
		if err := f.Close(); err != nil {
			return err
		}
		fmt.Printf("packed %s (%s) → %s + %s\nKeep %s next to %s.\n", m.Name, m.Source, output, target, filepath.Base(target), filepath.Base(output))
		return nil
	}
	if err := pack.Write(&payload, m, files); err != nil {
		return err
	}
	if err := pack.AppendTo(output, &payload); err != nil {
		return err
	}
	fmt.Printf("packed %s (%s) → %s (single file)\n", m.Name, m.Source, output)
	return nil
}

// copyFile copia src para dst com o modo dado (o stub precisa manter a assinatura e o
// entitlement de hypervisor, que estão dentro do próprio arquivo).
func copyFile(src, dst string, mode os.FileMode) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	os.Remove(dst)
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
	if err != nil {
		return err
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}

// packInspectCmd mostra o manifesto de um artefato sem extrair.
func packInspectCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "inspect PATH",
		Short: "Show what an artifact carries (manifest), without extracting it",
		Args:  cobra.ExactArgs(1),
		RunE: func(_ *cobra.Command, args []string) error {
			f, err := openPayload(args[0])
			if err != nil {
				return err
			}
			defer f.Close()
			m, _, err := pack.ReadManifest(f)
			if err != nil {
				return err
			}
			enc := json.NewEncoder(os.Stdout)
			enc.SetIndent("", "  ")
			return enc.Encode(m)
		},
	}
}

// packMode traduz a linha de comando de um artefato (executável com payload) para os
// subcomandos de máquina: run → machine run --from; start cria a máquina na 1ª vez; exec,
// stop, status, shell e logs vão à máquina do artefato. ok=false é o rc comum.
func packMode(exe string, args []string) ([]string, bool, error) {
	f, ok, err := pack.Locate(exe)
	if err != nil || !ok {
		return nil, false, err
	}
	m, _, err := pack.ReadManifest(f)
	f.Close()
	if err != nil {
		return nil, true, err
	}
	args, err = pack.Route(m, exe, args)
	return args, true, err
}

// ensurePackMachine cria a máquina persistente do artefato na primeira vez (start).
func ensurePackMachine(ctx context.Context, m *app.Machines, from, name string) error {
	if _, err := m.Status(name); err == nil {
		return nil
	}
	imp, err := importPack(from)
	if err != nil {
		return err
	}
	mc := imp.machine
	mc.Name = name
	_, err = m.CreateFrom(ctx, mc, imp.disk)
	return err
}

// buildImage é o BuildImage das máquinas: pack vem do cache, o resto é build de verdade.
func buildImage(ctx context.Context, d *deps, l domain.Language) error {
	if strings.HasPrefix(l.Image, packRef) {
		return packBuildImage(d, l)
	}
	_, err := d.builder.Build(ctx, l)
	return err
}

// inUse devolve as stacks e as chaves de pack das máquinas existentes: o que o prune não pode
// apagar.
func inUse(m *app.Machines) (stacks, packs map[string]bool, err error) {
	ms, err := m.List()
	if err != nil {
		return nil, nil, err
	}
	stacks, packs = map[string]bool{}, map[string]bool{}
	for _, mc := range ms {
		stacks[mc.Stack] = true
		if key, ok := strings.CutPrefix(mc.Image, packRef); ok {
			packs[key] = true
		}
	}
	return stacks, packs, nil
}

// packPruneCmd apaga as extrações de pack do cache: ficam as 5 usadas por último e as que
// alguma máquina usa; --all deixa só as usadas.
func packPruneCmd() *cobra.Command {
	var all bool
	c := &cobra.Command{
		Use: "prune", Short: "Remove cached pack extractions (keeps the 5 most recently used; --all keeps only those in use)",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			_, packs, err := inUse(d.newMachines())
			if err != nil {
				return err
			}
			removed, err := pack.Prune(packsDir(), pack.KeepRecent, packs, all)
			fmt.Printf("removed %d pack extraction(s)\n", len(removed))
			return err
		},
	}
	c.Flags().BoolVar(&all, "all", false, "remove every extraction no machine uses")
	return c
}

// imagePruneCmd libera o cache de imagens (o `machine prune`): build velho sempre;
// com --all, imagens de referência e de pack que nenhuma máquina usa. --name é aceito por
// compatibilidade (prune por máquina) e não muda nada: aqui o cache é do host.
func imagePruneCmd(use string) *cobra.Command {
	var all bool
	c := &cobra.Command{
		Use: use, Short: "Free the image cache: old builds always; with --all, image and pack refs no machine uses",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			stacks, _, err := inUse(d.newMachines())
			if err != nil {
				return err
			}
			removed, freed, err := d.builder.Prune(stacks, all)
			fmt.Printf("removed %d image file(s), freed %d MiB\n", len(removed), freed>>20)
			return err
		},
	}
	c.Flags().BoolVar(&all, "all", false, "also drop image and pack refs that no machine uses")
	c.Flags().String("name", "", "accepted for compatibility; the image cache is host-wide here")
	return c
}
