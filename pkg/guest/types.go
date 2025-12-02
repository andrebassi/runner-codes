package guest

// File represents a source file for multi-file execution
type File struct {
	Name    string `json:"name"`    // Filename (e.g., "main.py", "utils.py")
	Content string `json:"content"` // File contents
}

// Job represents a code execution request (internal format)
type Job struct {
	TraceID string            `json:"trace_id"`
	Lang    string            `json:"lang"`
	Code    string            `json:"code"`            // Simple single-file code
	Files   []File            `json:"files,omitempty"` // Multi-file support
	Stdin   interface{}       `json:"stdin,omitempty"` // string or []string for batch
	Timeout int               `json:"timeout"`
	Meta    map[string]string `json:"meta,omitempty"`
}

// Result represents the execution result
type Result struct {
	TraceID       string      `json:"trace_id,omitempty"`
	Status        string      `json:"status"`                   // "success" or "failed"
	Stdout        string      `json:"stdout"`                   // Standard output
	Stderr        interface{} `json:"stderr"`                   // Standard error (null if empty)
	Exception     interface{} `json:"exception"`                // Exception details (null if none)
	ExecutionTime int64       `json:"executionTime"`            // Time in milliseconds
	ExitCode      int         `json:"exit_code,omitempty"`      // Exit code (internal use)
	Error         string      `json:"error,omitempty"`          // Error message (E001-E006 format)
	Stdin         string      `json:"stdin,omitempty"`          // Echo stdin for batch results
}

// RunRequest represents the /run API request format
type RunRequest struct {
	Language string      `json:"language" binding:"required"`
	Stdin    interface{} `json:"stdin,omitempty"` // string or []string for batch
	Files    []File      `json:"files" binding:"required"`
}

// RunResponse represents a single execution response
type RunResponse struct {
	Status        string      `json:"status"`        // "success" or "failed"
	Stdout        string      `json:"stdout"`        // Standard output
	Stderr        interface{} `json:"stderr"`        // null if empty
	Exception     interface{} `json:"exception"`     // null if none, or error details
	ExecutionTime int64       `json:"executionTime"` // Time in milliseconds
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"` // Echo stdin for batch
}

// BatchResponse is an array of RunResponse for batch execution
type BatchResponse []RunResponse

// Error codes
const (
	ErrOperationTimeout  = "E001: operation timed out"
	ErrQuotaExceeded     = "E002: API quota exceeded"
	ErrInvalidToken      = "E003: invalid access_token"
	ErrMissingToken      = "E004: access_token missing"
	ErrStdinTooLong      = "E005: stdin too long"
	ErrUnsupportedLang   = "E006: unsupported language %s"
)

// LanguageConfig holds configuration for a supported language
type LanguageConfig struct {
	Name            string
	Extension       string
	Command         string
	Args            []string
	Env             []string // Environment variables to set (e.g., "JAVA_HOME=/opt/java/openjdk")
	NeedsBuild      bool
	BuildCmd        string
	BuildArgs       []string
	BuildArgConcat  bool   // If true, concatenate last BuildArg with output path (e.g., -o{path} instead of -o {path})
	UseStdinForCode bool   // If true, pass code via stdin instead of file argument (e.g., sqlite3)
	IsJVM           bool   // If true, use JVM-specific compilation (javac/java, kotlinc/kotlin, scalac/scala)
	JVMSourceName   string // Source file name without extension for JVM languages (default: "Main")
	JVMClassName    string // Class name to run for JVM languages (default: same as JVMSourceName)
}
