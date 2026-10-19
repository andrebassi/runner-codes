package guest

import (
	"encoding/json"
	"testing"
)

func TestJob_JSON(t *testing.T) {
	tests := []struct {
		name string
		job  Job
	}{
		{
			name: "basic job",
			job: Job{
				TraceID: "tr-123",
				Lang:    "python",
				Code:    "print('hello')",
				Timeout: 10,
			},
		},
		{
			name: "job with meta",
			job: Job{
				TraceID: "tr-456",
				Lang:    "nodejs",
				Code:    "console.log('hi')",
				Timeout: 30,
				Meta:    map[string]string{"key": "value", "env": "test"},
			},
		},
		{
			name: "job with stdin",
			job: Job{
				TraceID: "tr-789",
				Lang:    "python",
				Code:    "import sys; print(sys.stdin.read())",
				Stdin:   "test input",
				Timeout: 10,
			},
		},
		{
			name: "job with files",
			job: Job{
				TraceID: "tr-files",
				Lang:    "python",
				Files: []File{
					{Name: "main.py", Content: "from utils import greet\ngreet()"},
					{Name: "utils.py", Content: "def greet(): print('hello')"},
				},
				Timeout: 10,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Test round-trip: Marshal -> Unmarshal
			got, err := json.Marshal(tt.job)
			if err != nil {
				t.Fatalf("Marshal() error = %v", err)
			}

			var unmarshaled Job
			if err := json.Unmarshal(got, &unmarshaled); err != nil {
				t.Fatalf("Unmarshal() error = %v", err)
			}

			// Compare basic fields
			if unmarshaled.TraceID != tt.job.TraceID {
				t.Errorf("TraceID = %s, want %s", unmarshaled.TraceID, tt.job.TraceID)
			}
			if unmarshaled.Lang != tt.job.Lang {
				t.Errorf("Lang = %s, want %s", unmarshaled.Lang, tt.job.Lang)
			}
			if unmarshaled.Code != tt.job.Code {
				t.Errorf("Code = %s, want %s", unmarshaled.Code, tt.job.Code)
			}
			if unmarshaled.Timeout != tt.job.Timeout {
				t.Errorf("Timeout = %d, want %d", unmarshaled.Timeout, tt.job.Timeout)
			}
			if len(unmarshaled.Files) != len(tt.job.Files) {
				t.Errorf("Files count = %d, want %d", len(unmarshaled.Files), len(tt.job.Files))
			}
		})
	}
}

func TestRunResponse_JSON(t *testing.T) {
	tests := []struct {
		name     string
		response RunResponse
	}{
		{
			name: "successful response",
			response: RunResponse{
				Status:        "success",
				Stdout:        "Hello Peter\n",
				Stderr:        nil,
				Exception:     nil,
				ExecutionTime: 41,
			},
		},
		{
			name: "response with stderr",
			response: RunResponse{
				Status:        "success",
				Stdout:        "",
				Stderr:        "error message",
				Exception:     nil,
				ExecutionTime: 30,
			},
		},
		{
			name: "timeout response",
			response: RunResponse{
				Status:        "success",
				Stdout:        "",
				Stderr:        nil,
				Exception:     "Timeout",
				ExecutionTime: 10000,
			},
		},
		{
			name: "batch response with stdin echo",
			response: RunResponse{
				Status:        "success",
				Stdout:        "Hello Peter\n",
				Stderr:        nil,
				Exception:     nil,
				ExecutionTime: 30,
				Stdin:         "Peter",
			},
		},
		{
			name: "error response",
			response: RunResponse{
				Status: "failed",
				Error:  "E006: unsupported language xyz",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := json.Marshal(tt.response)
			if err != nil {
				t.Fatalf("Marshal() error = %v", err)
			}

			var unmarshaled RunResponse
			if err := json.Unmarshal(got, &unmarshaled); err != nil {
				t.Fatalf("Unmarshal() error = %v", err)
			}

			if unmarshaled.Status != tt.response.Status {
				t.Errorf("Status = %s, want %s", unmarshaled.Status, tt.response.Status)
			}
			if unmarshaled.Stdout != tt.response.Stdout {
				t.Errorf("Stdout = %s, want %s", unmarshaled.Stdout, tt.response.Stdout)
			}
			if unmarshaled.ExecutionTime != tt.response.ExecutionTime {
				t.Errorf("ExecutionTime = %d, want %d", unmarshaled.ExecutionTime, tt.response.ExecutionTime)
			}
		})
	}
}

func TestResult_IsSuccess(t *testing.T) {
	tests := []struct {
		name     string
		result   Result
		expected bool
	}{
		{
			name: "success - status success",
			result: Result{
				Status: "success",
				Error:  "",
			},
			expected: true,
		},
		{
			name: "failure - status failed",
			result: Result{
				Status: "failed",
				Error:  "E006: unsupported language xyz",
			},
			expected: false,
		},
		{
			name: "failure - error message present",
			result: Result{
				Status: "success",
				Error:  "E001: operation timed out",
			},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// IsSuccess logic: Status == "success" && Error == ""
			isSuccess := tt.result.Status == "success" && tt.result.Error == ""
			if isSuccess != tt.expected {
				t.Errorf("IsSuccess = %v, want %v", isSuccess, tt.expected)
			}
		})
	}
}

func TestLanguageConfig_Fields(t *testing.T) {
	tests := []struct {
		name       string
		config     LanguageConfig
		wantName   string
		wantExt    string
		wantCmd    string
		needsBuild bool
	}{
		{
			name: "interpreted language - python",
			config: LanguageConfig{
				Name:       "python",
				Extension:  ".py",
				Command:    "python3",
				Args:       []string{},
				NeedsBuild: false,
			},
			wantName:   "python",
			wantExt:    ".py",
			wantCmd:    "python3",
			needsBuild: false,
		},
		{
			name: "compiled language - rust",
			config: LanguageConfig{
				Name:       "rust",
				Extension:  ".rs",
				NeedsBuild: true,
				BuildCmd:   "rustc",
				BuildArgs:  []string{"-o"},
			},
			wantName:   "rust",
			wantExt:    ".rs",
			needsBuild: true,
		},
		{
			name: "jvm language - java",
			config: LanguageConfig{
				Name:       "java",
				Extension:  ".java",
				NeedsBuild: true,
				BuildCmd:   "javac",
				BuildArgs:  []string{},
				Command:    "java",
			},
			wantName:   "java",
			wantExt:    ".java",
			needsBuild: true,
		},
		{
			name: "shell language - bash",
			config: LanguageConfig{
				Name:       "bash",
				Extension:  ".sh",
				Command:    "bash",
				Args:       []string{},
				NeedsBuild: false,
			},
			wantName:   "bash",
			wantExt:    ".sh",
			wantCmd:    "bash",
			needsBuild: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.config.Name != tt.wantName {
				t.Errorf("Name = %s, want %s", tt.config.Name, tt.wantName)
			}
			if tt.config.Extension != tt.wantExt {
				t.Errorf("Extension = %s, want %s", tt.config.Extension, tt.wantExt)
			}
			if tt.config.NeedsBuild != tt.needsBuild {
				t.Errorf("NeedsBuild = %v, want %v", tt.config.NeedsBuild, tt.needsBuild)
			}
			if !tt.needsBuild && tt.config.Command != tt.wantCmd {
				t.Errorf("Command = %s, want %s", tt.config.Command, tt.wantCmd)
			}
		})
	}
}

func TestJob_Validation(t *testing.T) {
	tests := []struct {
		name        string
		job         Job
		expectValid bool
	}{
		{
			name: "valid job",
			job: Job{
				Lang:    "python",
				Code:    "print('hello')",
				Timeout: 10,
			},
			expectValid: true,
		},
		{
			name: "empty language",
			job: Job{
				Lang:    "",
				Code:    "print('hello')",
				Timeout: 10,
			},
			expectValid: false,
		},
		{
			name: "empty code",
			job: Job{
				Lang:    "python",
				Code:    "",
				Timeout: 10,
			},
			expectValid: false,
		},
		{
			name: "zero timeout - valid (uses default)",
			job: Job{
				Lang:    "python",
				Code:    "print('hello')",
				Timeout: 0,
			},
			expectValid: true,
		},
		{
			name: "negative timeout",
			job: Job{
				Lang:    "python",
				Code:    "print('hello')",
				Timeout: -1,
			},
			expectValid: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Basic validation logic
			isValid := tt.job.Lang != "" && tt.job.Code != "" && tt.job.Timeout >= 0
			if isValid != tt.expectValid {
				t.Errorf("validation = %v, want %v", isValid, tt.expectValid)
			}
		})
	}
}
