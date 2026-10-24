package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strconv"

	"github.com/spf13/cobra"

	"runner.codes/internal/adapters/screenctl"
	"runner.codes/internal/app"
)

// machineScreenCmd agrupa o controle da tela de uma máquina: captura e mouse e teclado, como uma pessoa na frente
// dela. Vai pelo visualizador da máquina (o mesmo da página no navegador), então não instala nada no guest.
func machineScreenCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use:   "screen",
		Short: "Control a machine's screen: screenshot, click, type, key, scroll (needs --display)",
		Long: "Control the screen of a running machine with a display, like a person in front of it.\n" +
			"Coordinates are pixels of the current screen (see `rc machine screen screenshot`), origin at the top left.",
	}
	c.PersistentFlags().StringVar(&name, "name", defaultMachineName(), "machine name")
	// do roda as ações na tela da máquina --name.
	do := func(actions ...screenctl.Action) func(*cobra.Command, []string) error {
		return withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			s, err := screenOf(m, name)
			if err != nil {
				return err
			}
			return s.Do(cmd.Context(), actions)
		})
	}
	var out string
	shot := &cobra.Command{
		Use: "screenshot", Short: "Save the current screen as JPEG and print its size",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			s, err := screenOf(m, name)
			if err != nil {
				return err
			}
			return saveShot(cmd.Context(), s, out)
		}),
	}
	shot.Flags().StringVarP(&out, "output", "o", "screen.jpg", "file to write (- writes the JPEG to stdout)")

	var button string
	var double bool
	click := &cobra.Command{
		Use: "click X Y", Short: "Move to (X, Y) and click", Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			xy, err := ints(args)
			if err != nil {
				return err
			}
			return do(screenctl.Action{Action: "click", X: xy[0], Y: xy[1], Button: button, Double: double})(cmd, args)
		},
	}
	click.Flags().StringVar(&button, "button", "left", "left, right or middle")
	click.Flags().BoolVar(&double, "double", false, "double click")

	move := &cobra.Command{
		Use: "move X Y", Short: "Move the pointer to (X, Y)", Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			xy, err := ints(args)
			if err != nil {
				return err
			}
			return do(screenctl.Action{Action: "move", X: xy[0], Y: xy[1]})(cmd, args)
		},
	}
	drag := &cobra.Command{
		Use: "drag X Y TO_X TO_Y", Short: "Press at (X, Y), drag and release at (TO_X, TO_Y)", Args: cobra.ExactArgs(4),
		RunE: func(cmd *cobra.Command, args []string) error {
			v, err := ints(args)
			if err != nil {
				return err
			}
			return do(screenctl.Action{Action: "drag", X: v[0], Y: v[1], ToX: v[2], ToY: v[3]})(cmd, args)
		},
	}
	typ := &cobra.Command{
		Use: "type TEXT", Short: "Type text on the keyboard (US layout, ASCII)", Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return do(screenctl.Action{Action: "type", Text: args[0]})(cmd, args)
		},
	}
	key := &cobra.Command{
		Use: "key COMBO", Short: "Press a key or combination: Return, ctrl+c, ctrl+alt+t, alt+F4", Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return do(screenctl.Action{Action: "key", Keys: args[0]})(cmd, args)
		},
	}
	var up bool
	var amount int
	scroll := &cobra.Command{
		Use: "scroll X Y", Short: "Scroll the wheel at (X, Y) (down unless --up)", Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			xy, err := ints(args)
			if err != nil {
				return err
			}
			dir := "down"
			if up {
				dir = "up"
			}
			return do(screenctl.Action{Action: "scroll", X: xy[0], Y: xy[1], Direction: dir, Amount: amount})(cmd, args)
		},
	}
	scroll.Flags().BoolVar(&up, "up", false, "scroll up")
	scroll.Flags().IntVar(&amount, "amount", 3, "wheel steps")

	var file string
	batch := &cobra.Command{
		Use: "do -f ACTIONS.json", Short: "Run a list of actions from a JSON file (- reads stdin)",
		Long: `The file is a JSON list of actions, the same as POST /api/v1/machines/:name/screen/actions:
  [{"action":"click","x":640,"y":400},{"action":"type","text":"ls -la"},{"action":"key","keys":"Return"}]`,
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			actions, err := readActions(file)
			if err != nil {
				return err
			}
			s, err := screenOf(m, name)
			if err != nil {
				return err
			}
			return s.Do(cmd.Context(), actions)
		}),
	}
	batch.Flags().StringVarP(&file, "file", "f", "-", "JSON file with the actions (- is stdin)")

	c.AddCommand(shot, click, move, drag, typ, key, scroll, batch)
	return c
}

// screenOf é o controle da tela de uma máquina ligada com display.
func screenOf(m *app.Machines, name string) (screenctl.Screen, error) {
	port, err := m.DisplayPort(name)
	if err != nil {
		return screenctl.Screen{}, err
	}
	return screenctl.ForPort(port), nil
}

// saveShot grava a captura (ou a manda ao stdout com "-") e imprime o tamanho no stderr.
func saveShot(ctx context.Context, s screenctl.Screen, out string) error {
	shot, err := s.Capture(ctx)
	if err != nil {
		return err
	}
	if out == "-" {
		_, err = os.Stdout.Write(shot.JPEG)
	} else {
		err = os.WriteFile(out, shot.JPEG, 0o644)
	}
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "screen %dx%d -> %s\n", shot.Width, shot.Height, out)
	return nil
}

// ints lê os argumentos como inteiros.
func ints(args []string) ([]int, error) {
	out := make([]int, len(args))
	for i, a := range args {
		n, err := strconv.Atoi(a)
		if err != nil {
			return nil, fmt.Errorf("%q is not an integer", a)
		}
		out[i] = n
	}
	return out, nil
}

// readActions lê a lista de ações em JSON de um arquivo ou do stdin.
func readActions(file string) ([]screenctl.Action, error) {
	var r io.Reader = os.Stdin
	if file != "-" {
		f, err := os.Open(file)
		if err != nil {
			return nil, err
		}
		defer f.Close()
		r = f
	}
	var actions []screenctl.Action
	dec := json.NewDecoder(r)
	dec.DisallowUnknownFields()
	if err := dec.Decode(&actions); err != nil {
		return nil, fmt.Errorf("actions JSON: %w", err)
	}
	return actions, nil
}
