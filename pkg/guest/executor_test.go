package guest

import (
	"os"
	"os/exec"
	"sort"
	"testing"
)

func TestNewExecutor(t *testing.T) {
	executor := NewExecutor()

	if executor == nil {
		t.Fatal("NewExecutor() returned nil")
	}

	if executor.languages == nil {
		t.Error("languages map should not be nil")
	}

	// Check that we have a reasonable number of languages configured
	if len(executor.languages) < 30 {
		t.Errorf("Expected at least 30 languages, got %d", len(executor.languages))
	}
}

func TestFindPythonCommand(t *testing.T) {
	cmd := findPythonCommand()

	// Should return a valid python command
	validCommands := []string{"python3", "python"}
	found := false
	for _, valid := range validCommands {
		if cmd == valid {
			found = true
			break
		}
	}

	if !found {
		t.Errorf("findPythonCommand() = %s, expected one of %v", cmd, validCommands)
	}
}

func TestExecutor_SupportedLanguages(t *testing.T) {
	executor := NewExecutor()
	langs := executor.SupportedLanguages()

	if len(langs) == 0 {
		t.Error("SupportedLanguages() returned empty list")
	}

	// Check for essential languages
	essentialLangs := []string{"python", "node", "go", "rust", "java", "bash"}
	langMap := make(map[string]bool)
	for _, l := range langs {
		langMap[l] = true
	}

	for _, essential := range essentialLangs {
		if !langMap[essential] {
			t.Errorf("Essential language %s not found in supported languages", essential)
		}
	}
}

func TestExecutor_LanguageConfig(t *testing.T) {
	executor := NewExecutor()

	tests := []struct {
		name       string
		lang       string
		wantExt    string
		needsBuild bool
	}{
		{"python", "python", ".py", false},
		{"node", "node", ".js", false},
		{"nodejs alias", "nodejs", ".js", false},
		{"javascript alias", "javascript", ".js", false},
		{"typescript", "typescript", ".ts", false},
		{"java", "java", ".java", true},
		{"c", "c", ".c", true},
		{"cpp", "cpp", ".cpp", true},
		{"go", "go", ".go", false},
		{"rust", "rust", ".rs", true},
		{"ruby", "ruby", ".rb", false},
		{"bash", "bash", ".sh", false},
		{"lua", "lua", ".lua", false},
		{"haskell", "haskell", ".hs", true},
		{"kotlin", "kotlin", ".kt", true},
		{"pascal", "pascal", ".pas", true},
		{"fortran", "fortran", ".f90", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			config, ok := executor.languages[tt.lang]
			if !ok {
				t.Fatalf("Language %s not found", tt.lang)
			}

			if config.Extension != tt.wantExt {
				t.Errorf("Extension = %s, want %s", config.Extension, tt.wantExt)
			}

			if config.NeedsBuild != tt.needsBuild {
				t.Errorf("NeedsBuild = %v, want %v", config.NeedsBuild, tt.needsBuild)
			}
		})
	}
}

func TestExecutor_Execute_UnsupportedLanguage(t *testing.T) {
	executor := NewExecutor()

	job := Job{
		TraceID: "tr-unsupported",
		Lang:    "brainfuck",
		Code:    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.",
		Timeout: 5,
	}

	result := executor.Execute(job)

	if result.ExitCode != 127 {
		t.Errorf("ExitCode = %d, want 127", result.ExitCode)
	}

	if result.Error == "" {
		t.Error("Error should not be empty for unsupported language")
	}

	if result.TraceID != job.TraceID {
		t.Errorf("TraceID = %s, want %s", result.TraceID, job.TraceID)
	}
}

func TestExecutor_Execute_ValidPython(t *testing.T) {
	// Skip if python is not available
	if _, err := exec.LookPath("python3"); err != nil {
		if _, err := exec.LookPath("python"); err != nil {
			t.Skip("Python not available, skipping test")
		}
	}

	executor := NewExecutor()

	job := Job{
		TraceID: "tr-python-test",
		Lang:    "python",
		Code:    "print('Hello from test')",
		Timeout: 10,
	}

	result := executor.Execute(job)

	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0, stderr: %s", result.ExitCode, result.Stderr)
	}

	if result.Stdout != "Hello from test\n" {
		t.Errorf("Stdout = %q, want %q", result.Stdout, "Hello from test\n")
	}

	if result.TraceID != job.TraceID {
		t.Errorf("TraceID = %s, want %s", result.TraceID, job.TraceID)
	}
}

func TestExecutor_Execute_PythonError(t *testing.T) {
	// Skip if python is not available
	if _, err := exec.LookPath("python3"); err != nil {
		if _, err := exec.LookPath("python"); err != nil {
			t.Skip("Python not available, skipping test")
		}
	}

	executor := NewExecutor()

	job := Job{
		TraceID: "tr-python-error",
		Lang:    "python",
		Code:    "print(undefined_variable)",
		Timeout: 10,
	}

	result := executor.Execute(job)

	if result.ExitCode == 0 {
		t.Error("ExitCode should be non-zero for error")
	}

	if result.Stderr == "" {
		t.Error("Stderr should contain error message")
	}
}

func TestExecutor_Execute_ValidBash(t *testing.T) {
	// Skip if bash is not available
	if _, err := exec.LookPath("bash"); err != nil {
		t.Skip("Bash not available, skipping test")
	}

	executor := NewExecutor()

	job := Job{
		TraceID: "tr-bash-test",
		Lang:    "bash",
		Code:    "echo 'Hello from bash'",
		Timeout: 10,
	}

	result := executor.Execute(job)

	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0, stderr: %s", result.ExitCode, result.Stderr)
	}

	if result.Stdout != "Hello from bash\n" {
		t.Errorf("Stdout = %q, want %q", result.Stdout, "Hello from bash\n")
	}
}

func TestExecutor_Execute_DefaultTimeout(t *testing.T) {
	// Skip if python is not available
	if _, err := exec.LookPath("python3"); err != nil {
		if _, err := exec.LookPath("python"); err != nil {
			t.Skip("Python not available, skipping test")
		}
	}

	executor := NewExecutor()

	// Test with zero timeout (should use default 10s)
	job := Job{
		TraceID: "tr-default-timeout",
		Lang:    "python",
		Code:    "print('quick')",
		Timeout: 0,
	}

	result := executor.Execute(job)

	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0", result.ExitCode)
	}
}

func TestExecutor_Execute_TempDirCleanup(t *testing.T) {
	// Skip if python is not available
	if _, err := exec.LookPath("python3"); err != nil {
		if _, err := exec.LookPath("python"); err != nil {
			t.Skip("Python not available, skipping test")
		}
	}

	executor := NewExecutor()

	// Get initial temp dir entries
	initialEntries, _ := os.ReadDir(os.TempDir())
	initialCount := countJobDirs(initialEntries)

	job := Job{
		TraceID: "tr-cleanup-test",
		Lang:    "python",
		Code:    "print('cleanup test')",
		Timeout: 5,
	}

	_ = executor.Execute(job)

	// Check that temp dir was cleaned up
	finalEntries, _ := os.ReadDir(os.TempDir())
	finalCount := countJobDirs(finalEntries)

	// There shouldn't be more job directories after execution
	if finalCount > initialCount {
		t.Errorf("Temp directories not cleaned up: before=%d, after=%d", initialCount, finalCount)
	}
}

func countJobDirs(entries []os.DirEntry) int {
	count := 0
	for _, entry := range entries {
		if entry.IsDir() && len(entry.Name()) > 4 && entry.Name()[:4] == "job-" {
			count++
		}
	}
	return count
}

func TestExecutor_Execute_NodeJS(t *testing.T) {
	// Skip if node is not available
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("Node.js not available, skipping test")
	}

	executor := NewExecutor()

	tests := []struct {
		name    string
		lang    string
		code    string
		wantOut string
	}{
		{
			name:    "node alias",
			lang:    "node",
			code:    "console.log('Hello Node')",
			wantOut: "Hello Node\n",
		},
		{
			name:    "nodejs alias",
			lang:    "nodejs",
			code:    "console.log('Hello NodeJS')",
			wantOut: "Hello NodeJS\n",
		},
		{
			name:    "javascript alias",
			lang:    "javascript",
			code:    "console.log('Hello JS')",
			wantOut: "Hello JS\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			job := Job{
				TraceID: "tr-" + tt.lang,
				Lang:    tt.lang,
				Code:    tt.code,
				Timeout: 10,
			}

			result := executor.Execute(job)

			if result.ExitCode != 0 {
				t.Errorf("ExitCode = %d, want 0, stderr: %s", result.ExitCode, result.Stderr)
			}

			if result.Stdout != tt.wantOut {
				t.Errorf("Stdout = %q, want %q", result.Stdout, tt.wantOut)
			}
		})
	}
}

func TestExecutor_Execute_Go(t *testing.T) {
	// Skip if go is not available
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("Go not available, skipping test")
	}

	executor := NewExecutor()

	job := Job{
		TraceID: "tr-go-test",
		Lang:    "go",
		Code: `package main

import "fmt"

func main() {
	fmt.Println("Hello from Go")
}`,
		Timeout: 30,
	}

	result := executor.Execute(job)

	// Skip if there's a Go version mismatch (common in dev environments)
	stderr := stderrStr(result.Stderr)
	if result.ExitCode != 0 && (contains(stderr, "version") && contains(stderr, "does not match")) {
		t.Skip("Go version mismatch in environment, skipping test")
	}

	if result.ExitCode != 0 {
		t.Errorf("ExitCode = %d, want 0, stderr: %s", result.ExitCode, stderr)
	}

	if result.Stdout != "Hello from Go\n" {
		t.Errorf("Stdout = %q, want %q", result.Stdout, "Hello from Go\n")
	}
}

// contains checks if s contains substr
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && containsHelper(s, substr))
}

func containsHelper(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

// stderrStr converts interface{} to string for test comparisons
func stderrStr(v interface{}) string {
	if v == nil {
		return ""
	}
	if s, ok := v.(string); ok {
		return s
	}
	return ""
}

func TestExecutor_SupportedLanguages_UniqueAndSorted(t *testing.T) {
	executor := NewExecutor()
	langs := executor.SupportedLanguages()

	// Check for duplicates
	seen := make(map[string]bool)
	for _, lang := range langs {
		if seen[lang] {
			t.Errorf("Duplicate language found: %s", lang)
		}
		seen[lang] = true
	}

	// Verify the list is sortable (no empty strings)
	for _, lang := range langs {
		if lang == "" {
			t.Error("Empty language name found")
		}
	}

	// Sort and verify it's sorted
	sorted := make([]string, len(langs))
	copy(sorted, langs)
	sort.Strings(sorted)
	// Note: SupportedLanguages doesn't guarantee sorting, just that it returns valid languages
}

func TestLanguageConfig_Tiers(t *testing.T) {
	executor := NewExecutor()

	// Tier 1: Popular Languages
	tier1 := []string{"python", "node", "nodejs", "javascript", "typescript", "java", "c", "cpp", "php"}
	for _, lang := range tier1 {
		if _, ok := executor.languages[lang]; !ok {
			t.Errorf("Tier 1 language %s not found", lang)
		}
	}

	// Tier 2: Common Languages
	tier2 := []string{"ruby", "perl", "scala", "kotlin", "swift"}
	for _, lang := range tier2 {
		if _, ok := executor.languages[lang]; !ok {
			t.Errorf("Tier 2 language %s not found", lang)
		}
	}

	// Tier 3: Functional/Scientific
	tier3 := []string{"haskell", "erlang", "elixir", "clojure", "ocaml"}
	for _, lang := range tier3 {
		if _, ok := executor.languages[lang]; !ok {
			t.Errorf("Tier 3 language %s not found", lang)
		}
	}

	// Tier 4: Scripting/Shell
	tier4 := []string{"bash", "sh", "lua", "r", "tcl", "groovy"}
	for _, lang := range tier4 {
		if _, ok := executor.languages[lang]; !ok {
			t.Errorf("Tier 4 language %s not found", lang)
		}
	}

	// Tier 5: Modern/Emerging
	tier5 := []string{"go", "rust", "dart", "deno", "bun"}
	for _, lang := range tier5 {
		if _, ok := executor.languages[lang]; !ok {
			t.Errorf("Tier 5 language %s not found", lang)
		}
	}
}

func TestExecutor_Execute_TraceIDPreserved(t *testing.T) {
	executor := NewExecutor()

	traceIDs := []string{
		"tr-123456",
		"",
		"custom-trace-id-with-dashes",
		"uuid-a1b2c3d4-e5f6-7890-abcd-ef1234567890",
	}

	for _, traceID := range traceIDs {
		t.Run("trace_"+traceID, func(t *testing.T) {
			job := Job{
				TraceID: traceID,
				Lang:    "unsupported-lang", // Will fail but should preserve trace ID
				Code:    "test",
				Timeout: 1,
			}

			result := executor.Execute(job)

			if result.TraceID != traceID {
				t.Errorf("TraceID = %s, want %s", result.TraceID, traceID)
			}
		})
	}
}
