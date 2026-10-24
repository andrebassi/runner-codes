package guest

import "runner.codes/internal/protocol"

// File, Job e Result são os tipos do protocolo host↔guest; o alias mantém o executor
// intacto enquanto a definição vive num lugar só (internal/protocol).
type (
	File   = protocol.File
	Job    = protocol.Job
	Result = protocol.Result
)

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
	ErrOperationTimeout = "E001: operation timed out"
	ErrQuotaExceeded    = "E002: API quota exceeded"
	ErrInvalidToken     = "E003: invalid access_token"
	ErrMissingToken     = "E004: access_token missing"
	ErrStdinTooLong     = "E005: stdin too long"
	ErrUnsupportedLang  = "E006: unsupported language %s"
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
