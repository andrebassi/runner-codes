package guest

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"
)

var (
	procMounted     bool
	procMountedOnce sync.Once
)

const (
	// MaxStdinLength is the maximum allowed stdin length (1MB)
	MaxStdinLength = 1_000_000
)

// Executor handles code execution for different languages
type Executor struct {
	languages map[string]LanguageConfig
}

// findPythonCommand detects available Python interpreter
func findPythonCommand() string {
	// Try absolute paths first (more reliable), then fallback to PATH lookup
	for _, cmd := range []string{"/usr/local/bin/python3", "/usr/bin/python3", "/usr/local/bin/python", "/usr/bin/python", "python3", "python"} {
		if _, err := exec.LookPath(cmd); err == nil {
			return cmd
		}
	}
	return "/usr/bin/python3" // Fallback
}

// findNodeCommand detects available Node.js interpreter
func findNodeCommand() string {
	// Try absolute paths first (Docker official images use /usr/local/bin)
	for _, cmd := range []string{"/usr/local/bin/node", "/usr/bin/node", "node"} {
		if _, err := exec.LookPath(cmd); err == nil {
			return cmd
		}
	}
	return "/usr/local/bin/node" // Fallback (Docker official images)
}

// NewExecutor creates a new code executor with support for 50+ languages
func NewExecutor() *Executor {
	pythonCmd := findPythonCommand()
	nodeCmd := findNodeCommand()
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
				Command:   nodeCmd,
				Args:      []string{},
			},
			"nodejs": {
				Name:      "nodejs",
				Extension: ".js",
				Command:   nodeCmd,
				Args:      []string{},
			},
			"javascript": {
				Name:      "javascript",
				Extension: ".js",
				Command:   nodeCmd,
				Args:      []string{},
			},
			"typescript": {
				Name:      "typescript",
				Extension: ".ts",
				Command:   "ts-node",
				Args:      []string{},
			},
			"java": {
				Name:         "java",
				Extension:    ".java",
				IsJVM:        true,
				BuildCmd:     "javac",
				BuildArgs:    []string{},
				Command:      "java",
				Args:         []string{},
				JVMClassName: "Main",
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
				Name:         "scala",
				Extension:    ".scala",
				IsJVM:        true,
				BuildCmd:     "scalac",
				BuildArgs:    []string{},
				Command:      "scala",
				Args:         []string{},
				JVMClassName: "Main",
			},
			"kotlin": {
				Name:          "kotlin",
				Extension:     ".kt",
				IsJVM:         true,
				BuildCmd:      "kotlinc",
				BuildArgs:     []string{},
				Command:       "kotlin",
				Args:          []string{},
				JVMSourceName: "Main",   // Source file: Main.kt
				JVMClassName:  "MainKt", // Kotlin compiles Main.kt to MainKt class
			},
			"swift": {
				Name:      "swift",
				Extension: ".swift",
				Command:   "swift",
				Args:      []string{},
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
				Command:   "/opt/groovy/bin/groovy",
				Args:      []string{},
				Env:       []string{"JAVA_HOME=/opt/java/openjdk", "PATH=/opt/java/openjdk/bin:/opt/groovy/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"},
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
				BuildArgConcat: true, // fpc requires -o{path} without space
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

			// ===================================================================
			// Tier 7: Logic/Specialized Languages
			// ===================================================================
			"prolog": {
				Name:      "prolog",
				Extension: ".pl",
				Command:   "swipl",
				Args:      []string{"-q", "-t", "halt", "-s"},
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
			// Tier 8: Specialized/Domain Languages
			// ===================================================================
			"d": {
				Name:           "d",
				Extension:      ".d",
				NeedsBuild:     true,
				BuildCmd:       "ldc2",
				BuildArgs:      []string{"-of="},
				BuildArgConcat: true, // ldc2 requires -of={path} without space
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
			// Tier 9: Microsoft/.NET Languages
			// ===================================================================
			"csharp": {
				Name:      "csharp",
				Extension: ".cs",
				Command:   "/usr/local/bin/csharp-run",
				Args:      []string{},
			},
			"dotnet": {
				Name:      "dotnet",
				Extension: ".cs",
				Command:   "/usr/local/bin/csharp-run",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 10: Special/Utility
			// ===================================================================
			"sqlite": {
				Name:            "sqlite",
				Extension:       ".sql",
				Command:         "sqlite3",
				Args:            []string{":memory:"},
				UseStdinForCode: true,
			},
			"awk": {
				Name:      "awk",
				Extension: ".awk",
				Command:   "awk",
				Args:      []string{"-f"},
			},
			"jq": {
				Name:      "jq",
				Extension: ".jq",
				Command:   "jq",
				Args:      []string{"--null-input", "-f"},
			},
			"nasm": {
				Name:      "nasm",
				Extension: ".asm",
				Command:   "/usr/local/bin/nasm-run",
				Args:      []string{},
			},
			"nim": {
				Name:      "nim",
				Extension: ".nim",
				Command:   "nim",
				Args:      []string{"compile", "--run"},
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
			"base": {
				Name:      "base",
				Extension: ".sh",
				Command:   "bash",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 10: Database Languages
			// ===================================================================
			"mysql": {
				Name:      "mysql",
				Extension: ".sql",
				Command:   "/usr/local/bin/mysql-run",
				Args:      []string{},
			},
			"postgresql": {
				Name:      "postgresql",
				Extension: ".sql",
				Command:   "/usr/local/bin/psql-run",
				Args:      []string{},
			},
			"postgres": {
				Name:      "postgres",
				Extension: ".sql",
				Command:   "/usr/local/bin/psql-run",
				Args:      []string{},
			},
			"oracle": {
				Name:      "oracle",
				Extension: ".sql",
				Command:   "/usr/local/bin/oracle-run",
				Args:      []string{},
			},

			// ===================================================================
			// Tier 11: SQL Syntax Validators (node-sql-parser based)
			// These validate SQL syntax WITHOUT requiring a real database
			// ===================================================================
			"mysql-parser": {
				Name:      "mysql-parser",
				Extension: ".sql",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/mysql-run"},
				Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
			},
			"postgresql-parser": {
				Name:      "postgresql-parser",
				Extension: ".sql",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/psql-run"},
				Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
			},
			"mariadb-parser": {
				Name:      "mariadb-parser",
				Extension: ".sql",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/mariadb-run"},
				Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
			},
			"sqlite-parser": {
				Name:      "sqlite-parser",
				Extension: ".sql",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/sqlite-run"},
				Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
			},

			// ===================================================================
			// Tier 12: NoSQL Query Validators
			// ===================================================================
			"mongodb-parser": {
				Name:      "mongodb-parser",
				Extension: ".json",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/mongodb-run"},
			},
			"redis-parser": {
				Name:      "redis-parser",
				Extension: ".redis",
				Command:   "/usr/local/bin/node",
				Args:      []string{"/usr/local/bin/redis-run"},
			},
		},
	}
}

// ensureProc mounts /proc if not already mounted
// This is necessary after Firecracker snapshot restore, as /proc is not persisted
func ensureProc() {
	procMountedOnce.Do(func() {
		// Check if /proc is already mounted
		if _, err := os.Stat("/proc/self"); err == nil {
			procMounted = true
			return
		}

		// Try to mount /proc
		fmt.Printf("[executor] Mounting /proc (required for compilers like rustc)...\n")
		err := syscall.Mount("proc", "/proc", "proc", 0, "")
		if err != nil {
			// Try with exec as fallback (some systems need this)
			cmd := exec.Command("mount", "-t", "proc", "proc", "/proc")
			if execErr := cmd.Run(); execErr != nil {
				fmt.Printf("[executor] Warning: failed to mount /proc: %v (exec: %v)\n", err, execErr)
			} else {
				procMounted = true
				fmt.Printf("[executor] /proc mounted successfully via exec\n")
			}
		} else {
			procMounted = true
			fmt.Printf("[executor] /proc mounted successfully via syscall\n")
		}
	})
}

// Execute runs the code and returns the result
func (e *Executor) Execute(job Job) Result {
	fmt.Printf("[executor] Execute called for lang=%s trace_id=%s\n", job.Lang, job.TraceID)

	// Ensure /proc is mounted (needed after snapshot restore for compilers like rustc)
	ensureProc()

	result := Result{
		TraceID: job.TraceID,
		Status:  "success",
		Stderr:  nil, // null when empty
	}

	// Validate language
	langConfig, ok := e.languages[job.Lang]
	if !ok {
		fmt.Printf("[executor] Unsupported language: %s\n", job.Lang)
		result.Status = "failed"
		result.ExitCode = 127
		result.Error = fmt.Sprintf(ErrUnsupportedLang, job.Lang)
		return result
	}
	fmt.Printf("[executor] Language config found: cmd=%s args=%v\n", langConfig.Command, langConfig.Args)

	// Validate stdin length
	stdinStr := e.getStdinString(job.Stdin)
	if len(stdinStr) > MaxStdinLength {
		result.Status = "failed"
		result.ExitCode = 1
		result.Error = ErrStdinTooLong
		return result
	}

	// Create temp directory for execution
	fmt.Printf("[executor] Creating temp dir...\n")
	tmpDir, err := os.MkdirTemp("", "job-")
	if err != nil {
		fmt.Printf("[executor] Failed to create temp dir: %v\n", err)
		result.Status = "failed"
		result.ExitCode = 1
		result.Exception = fmt.Sprintf("create temp dir: %v", err)
		return result
	}
	fmt.Printf("[executor] Created temp dir: %s\n", tmpDir)
	defer os.RemoveAll(tmpDir)

	// Write files (multi-file support)
	fmt.Printf("[executor] Writing files...\n")
	mainFile, err := e.writeFiles(tmpDir, job, langConfig)
	if err != nil {
		fmt.Printf("[executor] Failed to write files: %v\n", err)
		result.Status = "failed"
		result.ExitCode = 1
		result.Exception = fmt.Sprintf("write files: %v", err)
		return result
	}
	fmt.Printf("[executor] Main file: %s\n", mainFile)

	// Set timeout
	timeout := time.Duration(job.Timeout) * time.Second
	if timeout <= 0 {
		timeout = 10 * time.Second
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	execStart := time.Now()

	// Handle JVM languages (Java, Kotlin, Scala) - special compilation flow
	if langConfig.IsJVM {
		className := langConfig.JVMClassName
		if className == "" {
			// Default: use source name if set, otherwise "Main"
			if langConfig.JVMSourceName != "" {
				className = langConfig.JVMSourceName
			} else {
				className = "Main"
			}
		}

		// Compile: javac/kotlinc/scalac Main.java/Main.kt/Main.scala
		buildArgs := append(langConfig.BuildArgs, mainFile)
		stdout, stderr, exitCode := e.runCommandWithStdin(ctx, tmpDir, "", langConfig.BuildCmd, buildArgs...)

		if exitCode != 0 {
			result.Status = "success"
			result.Stdout = stdout
			if stderr != "" {
				result.Stderr = stderr
			}
			result.Exception = "Compilation failed"
			result.ExecutionTime = time.Since(execStart).Milliseconds()
			return result
		}

		// Run: java/kotlin/scala Main (from the same directory)
		runArgs := append(langConfig.Args, className)
		stdout, stderr, exitCode = e.runCommandWithStdin(ctx, tmpDir, stdinStr, langConfig.Command, runArgs...)
		result.ExecutionTime = time.Since(execStart).Milliseconds()
		result.Stdout = stdout
		if stderr != "" {
			result.Stderr = stderr
		}
		result.ExitCode = exitCode

		if ctx.Err() == context.DeadlineExceeded {
			result.Exception = "Timeout"
		}
		return result
	}

	// Handle compiled languages (C, C++, Rust, Go, etc.)
	if langConfig.NeedsBuild {
		binFile := filepath.Join(tmpDir, "runbin")

		// Build args for compilation
		var buildArgs []string
		if langConfig.BuildArgConcat && len(langConfig.BuildArgs) > 0 {
			// Concatenate last build arg with output path (e.g., -o{path} instead of -o {path})
			buildArgs = append([]string{}, langConfig.BuildArgs[:len(langConfig.BuildArgs)-1]...)
			buildArgs = append(buildArgs, langConfig.BuildArgs[len(langConfig.BuildArgs)-1]+binFile, mainFile)
		} else {
			buildArgs = append(langConfig.BuildArgs, binFile, mainFile)
		}

		stdout, stderr, exitCode := e.runCommandWithStdin(ctx, tmpDir, "", langConfig.BuildCmd, buildArgs...)

		if exitCode != 0 {
			result.Status = "success" // API status is success, code failed
			result.Stdout = stdout
			if stderr != "" {
				result.Stderr = stderr
			}
			result.Exception = "Compilation failed"
			result.ExecutionTime = time.Since(execStart).Milliseconds()
			return result
		}

		// Run the compiled binary with stdin
		stdout, stderr, exitCode = e.runCommandWithStdin(ctx, tmpDir, stdinStr, binFile)
		result.ExecutionTime = time.Since(execStart).Milliseconds()
		result.Stdout = stdout
		if stderr != "" {
			result.Stderr = stderr
		}
		result.ExitCode = exitCode

		if ctx.Err() == context.DeadlineExceeded {
			result.Exception = "Timeout"
		}
		return result
	}

	// Run interpreted languages
	var args []string
	var inputStdin string
	if langConfig.UseStdinForCode {
		// For languages like sqlite3 that read code from stdin
		args = langConfig.Args
		// Read code from file and use as stdin (job stdin is prepended if provided)
		codeBytes, _ := os.ReadFile(mainFile)
		if stdinStr != "" {
			inputStdin = stdinStr + "\n" + string(codeBytes)
		} else {
			inputStdin = string(codeBytes)
		}
	} else {
		// Normal execution: pass file as argument
		args = append(langConfig.Args, mainFile)
		inputStdin = stdinStr
	}
	stdout, stderr, exitCode := e.runCommandWithStdin(ctx, tmpDir, inputStdin, langConfig.Command, args...)
	result.ExecutionTime = time.Since(execStart).Milliseconds()
	result.Stdout = stdout
	if stderr != "" {
		result.Stderr = stderr
	}
	result.ExitCode = exitCode

	if ctx.Err() == context.DeadlineExceeded {
		result.Exception = "Timeout"
	}

	return result
}

// ExecuteBatch runs the code multiple times with different stdin values
func (e *Executor) ExecuteBatch(job Job, stdinArray []string) []RunResponse {
	results := make([]RunResponse, 0, len(stdinArray))

	for _, stdin := range stdinArray {
		// Create a single job for this stdin
		singleJob := job
		singleJob.Stdin = stdin

		singleResult := e.Execute(singleJob)

		response := RunResponse{
			Status:        "success",
			Stdout:        singleResult.Stdout,
			Stderr:        singleResult.Stderr,
			Exception:     singleResult.Exception,
			ExecutionTime: singleResult.ExecutionTime,
			Stdin:         stdin,
		}

		results = append(results, response)
	}

	return results
}

// writeFiles writes all source files to the temp directory
func (e *Executor) writeFiles(tmpDir string, job Job, langConfig LanguageConfig) (string, error) {
	// If using simple code field (backward compatible)
	if job.Code != "" && len(job.Files) == 0 {
		// For JVM languages, use source name as filename (e.g., Main.java, Main.kt)
		var filename string
		if langConfig.IsJVM {
			// Use JVMSourceName if set, otherwise JVMClassName, otherwise "Main"
			sourceName := langConfig.JVMSourceName
			if sourceName == "" {
				sourceName = langConfig.JVMClassName
			}
			if sourceName == "" {
				sourceName = "Main"
			}
			filename = sourceName + langConfig.Extension
		} else {
			filename = "script" + langConfig.Extension
		}
		srcFile := filepath.Join(tmpDir, filename)
		if err := os.WriteFile(srcFile, []byte(job.Code), 0644); err != nil {
			return "", err
		}
		return srcFile, nil
	}

	// Multi-file support
	var mainFile string
	for _, file := range job.Files {
		filePath := filepath.Join(tmpDir, file.Name)

		// Create subdirectories if needed
		if dir := filepath.Dir(filePath); dir != tmpDir {
			if err := os.MkdirAll(dir, 0755); err != nil {
				return "", err
			}
		}

		if err := os.WriteFile(filePath, []byte(file.Content), 0644); err != nil {
			return "", err
		}

		if mainFile == "" {
			mainFile = filePath
		}
	}

	// If no main file specified, use the first file with the correct extension
	if mainFile == "" {
		for _, file := range job.Files {
			if strings.HasSuffix(file.Name, langConfig.Extension) {
				mainFile = filepath.Join(tmpDir, file.Name)
				break
			}
		}
	}

	return mainFile, nil
}

// getStdinString converts stdin interface to string
func (e *Executor) getStdinString(stdin interface{}) string {
	if stdin == nil {
		return ""
	}
	switch v := stdin.(type) {
	case string:
		return v
	case []byte:
		return string(v)
	default:
		return fmt.Sprintf("%v", v)
	}
}

// runCommand executes a command and returns stdout, stderr, and exit code
func (e *Executor) runCommand(ctx context.Context, dir string, name string, args ...string) (string, string, int) {
	return e.runCommandWithStdin(ctx, dir, "", name, args...)
}

// runCommandWithStdin executes a command with stdin support
func (e *Executor) runCommandWithStdin(ctx context.Context, dir string, stdin string, name string, args ...string) (string, string, int) {
	// Log command being executed
	fmt.Printf("[executor] Running: %s %v in dir: %s\n", name, args, dir)

	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Dir = dir

	// Set essential environment variables for languages like Go, Rust, Node.js, JVM
	cmd.Env = append(os.Environ(),
		"HOME=/tmp",
		"TMPDIR=/tmp",
		"GOCACHE=/tmp/go-cache",
		"GOPATH=/tmp/go",
		"GOROOT=/usr/local/go",
		"CARGO_HOME=/opt/cargo",
		"RUSTUP_HOME=/opt/rustup",
		// JVM languages (Java, Groovy, Clojure, Kotlin, Scala)
		"JAVA_HOME=/opt/java/openjdk",
		"GROOVY_HOME=/opt/groovy",
		"LD_LIBRARY_PATH=/opt/java/openjdk/lib:/opt/java/openjdk/lib/server",
		// PATH with all language binaries (including dotnet-script tools)
		"PATH=/root/.dotnet/tools:/usr/share/dotnet:/opt/java/openjdk/bin:/opt/groovy/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:/usr/local/sbin:/usr/local/go/bin:/opt/cargo/bin",
		// .NET specific settings
		"DOTNET_ROOT=/usr/share/dotnet",
		"DOTNET_CLI_TELEMETRY_OPTOUT=1",
		"DOTNET_NOLOGO=1",
		// Node.js/libuv specific settings for Firecracker
		"UV_USE_IO_URING=0",       // Disable io_uring which can cause issues in VMs
		"UV_THREADPOOL_SIZE=1",    // Minimize threads
		// node-sql-parser module path for SQL validators
		"NODE_PATH=/opt/sql-parser/node_modules",
		"NODE_NO_READLINE=1",      // Disable readline
		"NO_COLOR=1",              // Disable color output
		"FORCE_COLOR=0",           // Disable color output
	)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	// Provide stdin if available
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}

	fmt.Printf("[executor] Starting cmd.Run()...\n")
	err := cmd.Run()
	fmt.Printf("[executor] cmd.Run() completed, err=%v\n", err)

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
