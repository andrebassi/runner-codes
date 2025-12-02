package main

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// Executor handles code execution for different languages
type Executor struct {
	languages map[string]LanguageConfig
}

// findPythonCommand detects available Python interpreter
func findPythonCommand() string {
	// Try python3 first, then python
	for _, cmd := range []string{"python3", "python"} {
		if _, err := exec.LookPath(cmd); err == nil {
			return cmd
		}
	}
	return "python3" // Fallback, will fail gracefully
}

// NewExecutor creates a new code executor with support for 50+ languages
func NewExecutor() *Executor {
	pythonCmd := findPythonCommand()
	return &Executor{
		languages: map[string]LanguageConfig{
			// ===================================================================
			// Tier 1: High Priority (Popular Languages)
			// ===================================================================
			"python": {
				Name:      "python",
				Extension: ".py",
				Command:   pythonCmd,
				Args:      []string{},
			},
			"node": {
				Name:      "node",
				Extension: ".js",
				Command:   "node",
				Args:      []string{},
			},
			"javascript": {
				Name:      "javascript",
				Extension: ".js",
				Command:   "node",
				Args:      []string{},
			},
			"typescript": {
				Name:      "typescript",
				Extension: ".ts",
				Command:   "ts-node",
				Args:      []string{},
			},
			"java": {
				Name:       "java",
				Extension:  ".java",
				NeedsBuild: true,
				BuildCmd:   "javac",
				BuildArgs:  []string{},
				Command:    "java",
			},
			"c": {
				Name:       "c",
				Extension:  ".c",
				NeedsBuild: true,
				BuildCmd:   "gcc",
				BuildArgs:  []string{"-o"},
			},
			"cpp": {
				Name:       "cpp",
				Extension:  ".cpp",
				NeedsBuild: true,
				BuildCmd:   "g++",
				BuildArgs:  []string{"-o"},
			},
			"php": {
				Name:      "php",
				Extension: ".php",
				Command:   "php",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 2: Medium Priority (Common Languages)
			// ===================================================================
			"ruby": {
				Name:      "ruby",
				Extension: ".rb",
				Command:   "ruby",
				Args:      []string{},
			},
			"perl": {
				Name:      "perl",
				Extension: ".pl",
				Command:   "perl",
				Args:      []string{},
			},
			"scala": {
				Name:      "scala",
				Extension: ".scala",
				Command:   "scala",
				Args:      []string{},
			},
			"kotlin": {
				Name:       "kotlin",
				Extension:  ".kt",
				NeedsBuild: true,
				BuildCmd:   "kotlinc",
				BuildArgs:  []string{"-include-runtime", "-d"},
			},
			"swift": {
				Name:      "swift",
				Extension: ".swift",
				Command:   "swift",
				Args:      []string{},
			},
			"objectivec": {
				Name:       "objectivec",
				Extension:  ".m",
				NeedsBuild: true,
				BuildCmd:   "clang",
				BuildArgs:  []string{"-framework", "Foundation", "-o"},
			},

			// ===================================================================
			// Tier 3: Functional/Scientific Languages
			// ===================================================================
			"haskell": {
				Name:       "haskell",
				Extension:  ".hs",
				NeedsBuild: true,
				BuildCmd:   "ghc",
				BuildArgs:  []string{"-o"},
			},
			"erlang": {
				Name:      "erlang",
				Extension: ".erl",
				Command:   "escript",
				Args:      []string{},
			},
			"elixir": {
				Name:      "elixir",
				Extension: ".ex",
				Command:   "elixir",
				Args:      []string{},
			},
			"clojure": {
				Name:      "clojure",
				Extension: ".clj",
				Command:   "clojure",
				Args:      []string{"-M"},
			},
			"ocaml": {
				Name:      "ocaml",
				Extension: ".ml",
				Command:   "ocaml",
				Args:      []string{},
			},
			"fsharp": {
				Name:      "fsharp",
				Extension: ".fs",
				Command:   "dotnet",
				Args:      []string{"fsi"},
			},

			// ===================================================================
			// Tier 4: Scripting/Shell Languages
			// ===================================================================
			"bash": {
				Name:      "bash",
				Extension: ".sh",
				Command:   "bash",
				Args:      []string{},
			},
			"sh": {
				Name:      "sh",
				Extension: ".sh",
				Command:   "sh",
				Args:      []string{},
			},
			"lua": {
				Name:      "lua",
				Extension: ".lua",
				Command:   "lua",
				Args:      []string{},
			},
			"r": {
				Name:      "r",
				Extension: ".r",
				Command:   "Rscript",
				Args:      []string{},
			},
			"tcl": {
				Name:      "tcl",
				Extension: ".tcl",
				Command:   "tclsh",
				Args:      []string{},
			},
			"groovy": {
				Name:      "groovy",
				Extension: ".groovy",
				Command:   "groovy",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 5: Modern/Emerging Languages
			// ===================================================================
			"go": {
				Name:      "go",
				Extension: ".go",
				Command:   "go",
				Args:      []string{"run"},
			},
			"rust": {
				Name:       "rust",
				Extension:  ".rs",
				Command:    "",
				NeedsBuild: true,
				BuildCmd:   "rustc",
				BuildArgs:  []string{"-o"},
			},
			"dart": {
				Name:      "dart",
				Extension: ".dart",
				Command:   "dart",
				Args:      []string{"run"},
			},
			"deno": {
				Name:      "deno",
				Extension: ".ts",
				Command:   "deno",
				Args:      []string{"run"},
			},
			"bun": {
				Name:      "bun",
				Extension: ".js",
				Command:   "bun",
				Args:      []string{"run"},
			},

			// ===================================================================
			// Tier 6: Classic/Educational Languages
			// ===================================================================
			"pascal": {
				Name:       "pascal",
				Extension:  ".pas",
				NeedsBuild: true,
				BuildCmd:   "fpc",
				BuildArgs:  []string{"-o"},
			},
			"fortran": {
				Name:       "fortran",
				Extension:  ".f90",
				NeedsBuild: true,
				BuildCmd:   "gfortran",
				BuildArgs:  []string{"-o"},
			},
			"cobol": {
				Name:       "cobol",
				Extension:  ".cob",
				NeedsBuild: true,
				BuildCmd:   "cobc",
				BuildArgs:  []string{"-x", "-o"},
			},
			"ada": {
				Name:       "ada",
				Extension:  ".adb",
				NeedsBuild: true,
				BuildCmd:   "gnat",
				BuildArgs:  []string{"make", "-o"},
			},
			"basic": {
				Name:      "basic",
				Extension: ".bas",
				Command:   "freebasic",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 7: Assembly/Low-Level
			// ===================================================================
			"assembly": {
				Name:       "assembly",
				Extension:  ".asm",
				NeedsBuild: true,
				BuildCmd:   "nasm",
				BuildArgs:  []string{"-f", "elf64", "-o"},
			},

			// ===================================================================
			// Tier 8: Logic/Specialized Languages
			// ===================================================================
			"prolog": {
				Name:      "prolog",
				Extension: ".pl",
				Command:   "swipl",
				Args:      []string{"-q", "-t", "halt", "-s"},
			},
			"racket": {
				Name:      "racket",
				Extension: ".rkt",
				Command:   "racket",
				Args:      []string{},
			},
			"lisp": {
				Name:      "lisp",
				Extension: ".lisp",
				Command:   "sbcl",
				Args:      []string{"--script"},
			},
			"scheme": {
				Name:      "scheme",
				Extension: ".scm",
				Command:   "guile",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 9: Specialized/Domain Languages
			// ===================================================================
			"d": {
				Name:       "d",
				Extension:  ".d",
				NeedsBuild: true,
				BuildCmd:   "dmd",
				BuildArgs:  []string{"-of="},
			},
			"coffeescript": {
				Name:      "coffeescript",
				Extension: ".coffee",
				Command:   "coffee",
				Args:      []string{},
			},
			"octave": {
				Name:      "octave",
				Extension: ".m",
				Command:   "octave",
				Args:      []string{"--no-gui", "--quiet"},
			},
			"julia": {
				Name:      "julia",
				Extension: ".jl",
				Command:   "julia",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 10: Microsoft/.NET Languages
			// ===================================================================
			"csharp": {
				Name:      "csharp",
				Extension: ".cs",
				Command:   "dotnet",
				Args:      []string{"run", "--project"},
			},
			"dotnet": {
				Name:      "dotnet",
				Extension: ".cs",
				Command:   "dotnet",
				Args:      []string{"run", "--project"},
			},
			"vbnet": {
				Name:      "vbnet",
				Extension: ".vb",
				Command:   "dotnet",
				Args:      []string{"run"},
			},
			"powershell": {
				Name:      "powershell",
				Extension: ".ps1",
				Command:   "pwsh",
				Args:      []string{"-File"},
			},

			// ===================================================================
			// Tier 11: Esoteric/Fun Languages
			// ===================================================================
			"brainfuck": {
				Name:      "brainfuck",
				Extension: ".bf",
				Command:   "bf",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 12: Special/Utility
			// ===================================================================
			"text": {
				Name:      "text",
				Extension: ".txt",
				Command:   "cat",
				Args:      []string{},
			},
			"jshell": {
				Name:      "jshell",
				Extension: ".jsh",
				Command:   "jshell",
				Args:      []string{},
			},
			"sqlite": {
				Name:      "sqlite",
				Extension: ".sql",
				Command:   "sqlite3",
				Args:      []string{":memory:"},
			},
			"awk": {
				Name:      "awk",
				Extension: ".awk",
				Command:   "awk",
				Args:      []string{"-f"},
			},
			"sed": {
				Name:      "sed",
				Extension: ".sed",
				Command:   "sed",
				Args:      []string{"-f"},
			},
			"jq": {
				Name:      "jq",
				Extension: ".jq",
				Command:   "jq",
				Args:      []string{"-f"},
			},
			"nasm": {
				Name:       "nasm",
				Extension:  ".asm",
				NeedsBuild: true,
				BuildCmd:   "nasm",
				BuildArgs:  []string{"-f", "elf64", "-o"},
			},
			"nim": {
				Name:       "nim",
				Extension:  ".nim",
				NeedsBuild: true,
				BuildCmd:   "nim",
				BuildArgs:  []string{"compile", "--run", "-o:"},
			},
			"crystal": {
				Name:      "crystal",
				Extension: ".cr",
				Command:   "crystal",
				Args:      []string{"run"},
			},
			"zig": {
				Name:      "zig",
				Extension: ".zig",
				Command:   "zig",
				Args:      []string{"run"},
			},
			"vlang": {
				Name:      "vlang",
				Extension: ".v",
				Command:   "v",
				Args:      []string{"run"},
			},
		},
	}
}

// Execute runs the code and returns the result
func (e *Executor) Execute(job Job) Result {
	result := Result{
		TraceID: job.TraceID,
	}

	// Validate language
	langConfig, ok := e.languages[job.Lang]
	if !ok {
		result.ExitCode = 127
		result.Stderr = fmt.Sprintf("unsupported language: %s", job.Lang)
		result.Error = result.Stderr
		return result
	}

	// Create temp directory for execution
	tmpDir, err := os.MkdirTemp("", "job-")
	if err != nil {
		result.ExitCode = 1
		result.Stderr = fmt.Sprintf("create temp dir: %v", err)
		result.Error = result.Stderr
		return result
	}
	defer os.RemoveAll(tmpDir)

	// Write code to file
	srcFile := filepath.Join(tmpDir, "script"+langConfig.Extension)
	if err := os.WriteFile(srcFile, []byte(job.Code), 0644); err != nil {
		result.ExitCode = 1
		result.Stderr = fmt.Sprintf("write source file: %v", err)
		result.Error = result.Stderr
		return result
	}

	// Set timeout
	timeout := time.Duration(job.Timeout) * time.Second
	if timeout <= 0 {
		timeout = 10 * time.Second
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	// Handle compiled languages
	if langConfig.NeedsBuild {
		binFile := filepath.Join(tmpDir, "runbin")
		stdout, stderr, exitCode := e.runCommand(ctx, tmpDir, langConfig.BuildCmd,
			append(langConfig.BuildArgs, binFile, srcFile)...)

		if exitCode != 0 {
			result.Stdout = stdout
			result.Stderr = stderr
			result.ExitCode = exitCode
			result.Error = "compilation failed"
			return result
		}

		// Run the compiled binary
		stdout, stderr, exitCode = e.runCommand(ctx, tmpDir, binFile)
		result.Stdout = stdout
		result.Stderr = stderr
		result.ExitCode = exitCode
		return result
	}

	// Run interpreted languages
	args := append(langConfig.Args, srcFile)
	stdout, stderr, exitCode := e.runCommand(ctx, tmpDir, langConfig.Command, args...)
	result.Stdout = stdout
	result.Stderr = stderr
	result.ExitCode = exitCode

	return result
}

// runCommand executes a command and returns stdout, stderr, and exit code
func (e *Executor) runCommand(ctx context.Context, dir string, name string, args ...string) (string, string, int) {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Dir = dir

	// Set essential environment variables for languages like Go, Rust, Node.js
	cmd.Env = append(os.Environ(),
		"HOME=/tmp",
		"GOCACHE=/tmp/go-cache",
		"GOPATH=/tmp/go",
		"GOROOT=/usr/local/go",
		"CARGO_HOME=/opt/cargo",
		"RUSTUP_HOME=/opt/rustup",
		"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin:/opt/cargo/bin",
	)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()

	exitCode := 0
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			exitCode = exitErr.ExitCode()
		} else if ctx.Err() == context.DeadlineExceeded {
			exitCode = 124 // Standard timeout exit code
			stderr.WriteString("\nExecution timed out")
		} else {
			exitCode = 1
			stderr.WriteString(err.Error())
		}
	}

	return stdout.String(), stderr.String(), exitCode
}

// SupportedLanguages returns a list of supported language names
func (e *Executor) SupportedLanguages() []string {
	langs := make([]string, 0, len(e.languages))
	for lang := range e.languages {
		langs = append(langs, lang)
	}
	return langs
}
