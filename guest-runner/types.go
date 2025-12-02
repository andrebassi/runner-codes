package main

// Job represents a code execution request from the host
type Job struct {
	TraceID string            `json:"trace_id"`
	Lang    string            `json:"lang"`
	Code    string            `json:"code"`
	Timeout int               `json:"timeout"`
	Meta    map[string]string `json:"meta,omitempty"`
}

// Result represents the execution result sent back to the host
type Result struct {
	TraceID  string `json:"trace_id,omitempty"`
	Stdout   string `json:"stdout"`
	Stderr   string `json:"stderr"`
	ExitCode int    `json:"exit_code"`
	Error    string `json:"error,omitempty"`
}

// LanguageConfig holds configuration for a supported language
type LanguageConfig struct {
	Name       string
	Extension  string
	Command    string
	Args       []string
	NeedsBuild bool
	BuildCmd   string
	BuildArgs  []string
}
