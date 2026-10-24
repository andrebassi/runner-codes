package mcpserver

// tool é a descrição de uma ferramenta no tools/list: nome, o que faz e o esquema JSON dos argumentos.
type tool struct {
	Name        string         `json:"name"`
	Description string         `json:"description"`
	InputSchema map[string]any `json:"inputSchema"`
}

// object monta o esquema de um objeto com as propriedades e as obrigatórias, sem campo extra.
func object(props map[string]any, required ...string) map[string]any {
	s := map[string]any{"type": "object", "properties": props, "additionalProperties": false}
	if len(required) > 0 {
		s["required"] = required
	}
	return s
}

// intProp é uma propriedade inteira com descrição.
func intProp(desc string) map[string]any {
	return map[string]any{"type": "integer", "description": desc}
}

// textProp é uma propriedade de texto com descrição.
func textProp(desc string) map[string]any {
	return map[string]any{"type": "string", "description": desc}
}

// tools descreve as ferramentas. As descrições são para o agente: dizem a unidade das coordenadas e o limite do
// teclado, que é o que mais erra quem opera uma tela pela primeira vez.
func (s *Server) tools() []tool {
	x, y := intProp("x in pixels of the last screenshot"), intProp("y in pixels of the last screenshot")
	return []tool{
		{"screenshot", "Capture the screen of machine " + s.Machine + " (JPEG). Coordinates of the other tools are pixels of this image.",
			object(map[string]any{})},
		{"click", "Move the pointer to (x, y) and click.",
			object(map[string]any{"x": x, "y": y,
				"button": map[string]any{"type": "string", "enum": []string{"left", "right", "middle"}},
				"double": map[string]any{"type": "boolean", "description": "double click"}}, "x", "y")},
		{"move", "Move the pointer to (x, y) without clicking (hover).", object(map[string]any{"x": x, "y": y}, "x", "y")},
		{"drag", "Press the left button at (x, y), drag and release at (to_x, to_y).",
			object(map[string]any{"x": x, "y": y, "to_x": intProp("destination x"), "to_y": intProp("destination y")}, "x", "y", "to_x", "to_y")},
		{"type", "Type text on the keyboard into the focused window. US layout, ASCII only: for other characters use exec with xdotool type.",
			object(map[string]any{"text": textProp("text to type; \\n presses Enter")}, "text")},
		{"key", "Press a key or a combination: Return, Escape, Tab, BackSpace, ctrl+c, ctrl+shift+t, alt+F4, Up, Page_Down...",
			object(map[string]any{"keys": textProp("key or combination joined by +")}, "keys")},
		{"scroll", "Scroll the mouse wheel at (x, y).",
			object(map[string]any{"x": x, "y": y,
				"direction": map[string]any{"type": "string", "enum": []string{"up", "down"}},
				"amount":    intProp("wheel steps (default 3)")}, "x", "y")},
		{"actions", "Run several screen actions in order, in one call. Each item is {\"action\": click|move|drag|type|key|scroll|wait, ...} with the same fields as the single tools; wait takes ms.",
			object(map[string]any{"actions": map[string]any{"type": "array", "items": map[string]any{"type": "object"}}}, "actions")},
		{"exec", "Run a shell command in machine " + s.Machine + " (sh -c) and return exit code, stdout and stderr. GUI programs need DISPLAY=:0.",
			object(map[string]any{"command": textProp("shell command"), "timeout": intProp("seconds (default 60)")}, "command")},
	}
}
