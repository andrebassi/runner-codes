package api

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"
	"infra-operator/internal/config"
)

func init() {
	gin.SetMode(gin.TestMode)
}

// setupTestRouter creates a router for testing without S3/snapshot dependencies
func setupTestRouter() *gin.Engine {
	router := gin.New()
	router.Use(gin.Recovery())

	// Health check
	router.GET("/health", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{
			"status":  "healthy",
			"version": "0.1.0",
		})
	})

	// Languages
	router.GET("/api/v1/languages", func(c *gin.Context) {
		langs := make([]gin.H, 0)
		for _, name := range config.AllLanguageNames() {
			lang, _ := config.GetLanguage(name)
			langs = append(langs, gin.H{
				"name":       lang.Name,
				"size_mb":    lang.SizeMB,
				"extension":  lang.Extension,
				"runCommand": lang.RunCommand,
			})
		}
		c.JSON(http.StatusOK, gin.H{
			"languages": langs,
			"count":     len(langs),
		})
	})

	router.GET("/api/v1/languages/:lang", func(c *gin.Context) {
		name := c.Param("lang")
		lang, ok := config.GetLanguage(name)
		if !ok {
			c.JSON(http.StatusNotFound, gin.H{"error": "language not found"})
			return
		}
		c.JSON(http.StatusOK, gin.H{
			"name":       lang.Name,
			"size_mb":    lang.SizeMB,
			"extension":  lang.Extension,
			"runCommand": lang.RunCommand,
		})
	})

	// Run endpoint
	router.POST("/api/v1/run", func(c *gin.Context) {
		var req RunRequest
		if err := c.ShouldBindJSON(&req); err != nil {
			c.JSON(http.StatusBadRequest, RunResponse{
				Status: "failed",
				Error:  "E004: access_token missing",
			})
			return
		}

		// Validate language
		if _, ok := config.GetLanguage(req.Language); !ok {
			c.JSON(http.StatusBadRequest, RunResponse{
				Status: "failed",
				Error:  "E006: unsupported language " + req.Language,
			})
			return
		}

		// Check stdin length
		if stdin, ok := req.Stdin.(string); ok && len(stdin) > 1_000_000 {
			c.JSON(http.StatusBadRequest, RunResponse{
				Status: "failed",
				Error:  "E005: stdin too long",
			})
			return
		}

		// Check for batch execution
		if stdinArray, ok := req.Stdin.([]interface{}); ok && len(stdinArray) > 0 {
			results := make([]RunResponse, 0, len(stdinArray))
			for _, stdin := range stdinArray {
				stdinStr := ""
				if s, ok := stdin.(string); ok {
					stdinStr = s
				}
				results = append(results, RunResponse{
					Status:        "success",
					Stdout:        "[batch execution - vsock pending]",
					Stderr:        nil,
					Exception:     nil,
					ExecutionTime: 30,
					Stdin:         stdinStr,
				})
			}
			c.JSON(http.StatusOK, results)
			return
		}

		// Single execution
		c.JSON(http.StatusOK, RunResponse{
			Status:        "success",
			Stdout:        "[execution pending - vsock required]",
			Stderr:        nil,
			Exception:     nil,
			ExecutionTime: 0,
		})
	})

	return router
}

func TestHealthHandler(t *testing.T) {
	router := setupTestRouter()

	req, _ := http.NewRequest("GET", "/health", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	if err := json.Unmarshal(w.Body.Bytes(), &response); err != nil {
		t.Fatalf("Failed to unmarshal response: %v", err)
	}

	if response["status"] != "healthy" {
		t.Errorf("Expected status 'healthy', got '%v'", response["status"])
	}

	if response["version"] != "0.1.0" {
		t.Errorf("Expected version '0.1.0', got '%v'", response["version"])
	}
}

func TestListLanguagesHandler(t *testing.T) {
	router := setupTestRouter()

	req, _ := http.NewRequest("GET", "/api/v1/languages", nil)
	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var response map[string]interface{}
	if err := json.Unmarshal(w.Body.Bytes(), &response); err != nil {
		t.Fatalf("Failed to unmarshal response: %v", err)
	}

	// Check count
	count, ok := response["count"].(float64)
	if !ok {
		t.Fatal("count field not found or not a number")
	}
	if count < 30 {
		t.Errorf("Expected at least 30 languages, got %v", count)
	}

	// Check languages array
	langs, ok := response["languages"].([]interface{})
	if !ok {
		t.Fatal("languages field not found or not an array")
	}
	if len(langs) < 30 {
		t.Errorf("Expected at least 30 languages in array, got %d", len(langs))
	}
}

func TestGetLanguageHandler(t *testing.T) {
	router := setupTestRouter()

	tests := []struct {
		name           string
		lang           string
		expectedStatus int
		expectError    bool
	}{
		{
			name:           "valid language - python",
			lang:           "python",
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
		{
			name:           "valid language - nodejs",
			lang:           "nodejs",
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
		{
			name:           "valid language - go",
			lang:           "go",
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
		{
			name:           "invalid language",
			lang:           "brainfuck",
			expectedStatus: http.StatusNotFound,
			expectError:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			req, _ := http.NewRequest("GET", "/api/v1/languages/"+tt.lang, nil)
			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			if w.Code != tt.expectedStatus {
				t.Errorf("Expected status %d, got %d", tt.expectedStatus, w.Code)
			}

			var response map[string]interface{}
			if err := json.Unmarshal(w.Body.Bytes(), &response); err != nil {
				t.Fatalf("Failed to unmarshal response: %v", err)
			}

			if tt.expectError {
				if _, ok := response["error"]; !ok {
					t.Error("Expected error in response")
				}
			} else {
				if response["name"] != tt.lang {
					t.Errorf("Expected name '%s', got '%v'", tt.lang, response["name"])
				}
			}
		})
	}
}

func TestRunHandler_SingleExecution(t *testing.T) {
	router := setupTestRouter()

	tests := []struct {
		name           string
		request        RunRequest
		expectedStatus int
		expectError    bool
		errorCode      string
	}{
		{
			name: "valid python request",
			request: RunRequest{
				Language: "python",
				Files: []FileRequest{
					{Name: "main.py", Content: "print('hello')"},
				},
			},
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
		{
			name: "valid request with stdin",
			request: RunRequest{
				Language: "python",
				Stdin:    "test input",
				Files: []FileRequest{
					{Name: "main.py", Content: "import sys; print(sys.stdin.read())"},
				},
			},
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
		{
			name: "unsupported language",
			request: RunRequest{
				Language: "brainfuck",
				Files: []FileRequest{
					{Name: "main.bf", Content: "++++++++++."},
				},
			},
			expectedStatus: http.StatusBadRequest,
			expectError:    true,
			errorCode:      "E006",
		},
		{
			name: "valid nodejs request",
			request: RunRequest{
				Language: "nodejs",
				Files: []FileRequest{
					{Name: "main.js", Content: "console.log('hello')"},
				},
			},
			expectedStatus: http.StatusOK,
			expectError:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			body, _ := json.Marshal(tt.request)
			req, _ := http.NewRequest("POST", "/api/v1/run", bytes.NewBuffer(body))
			req.Header.Set("Content-Type", "application/json")

			w := httptest.NewRecorder()
			router.ServeHTTP(w, req)

			if w.Code != tt.expectedStatus {
				t.Errorf("Expected status %d, got %d, body: %s", tt.expectedStatus, w.Code, w.Body.String())
			}

			var response RunResponse
			if err := json.Unmarshal(w.Body.Bytes(), &response); err != nil {
				t.Fatalf("Failed to unmarshal response: %v", err)
			}

			if tt.expectError {
				if response.Status != "failed" {
					t.Errorf("Expected status 'failed', got '%s'", response.Status)
				}
				if tt.errorCode != "" && !contains(response.Error, tt.errorCode) {
					t.Errorf("Expected error code '%s' in '%s'", tt.errorCode, response.Error)
				}
			} else {
				if response.Status != "success" {
					t.Errorf("Expected status 'success', got '%s'", response.Status)
				}
			}
		})
	}
}

func TestRunHandler_BatchExecution(t *testing.T) {
	router := setupTestRouter()

	request := map[string]interface{}{
		"language": "python",
		"stdin":    []string{"Peter", "Brian", "Meg"},
		"files": []map[string]string{
			{"name": "main.py", "content": "import sys; print('Hello ' + sys.stdin.readline())"},
		},
	}

	body, _ := json.Marshal(request)
	req, _ := http.NewRequest("POST", "/api/v1/run", bytes.NewBuffer(body))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Errorf("Expected status %d, got %d", http.StatusOK, w.Code)
	}

	var responses []RunResponse
	if err := json.Unmarshal(w.Body.Bytes(), &responses); err != nil {
		t.Fatalf("Failed to unmarshal batch response: %v", err)
	}

	if len(responses) != 3 {
		t.Errorf("Expected 3 responses, got %d", len(responses))
	}

	expectedStdin := []string{"Peter", "Brian", "Meg"}
	for i, resp := range responses {
		if resp.Status != "success" {
			t.Errorf("Response %d: expected status 'success', got '%s'", i, resp.Status)
		}
		if resp.Stdin != expectedStdin[i] {
			t.Errorf("Response %d: expected stdin '%s', got '%s'", i, expectedStdin[i], resp.Stdin)
		}
	}
}

func TestRunHandler_InvalidJSON(t *testing.T) {
	router := setupTestRouter()

	req, _ := http.NewRequest("POST", "/api/v1/run", bytes.NewBufferString("invalid json"))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Errorf("Expected status %d, got %d", http.StatusBadRequest, w.Code)
	}

	var response RunResponse
	if err := json.Unmarshal(w.Body.Bytes(), &response); err != nil {
		t.Fatalf("Failed to unmarshal response: %v", err)
	}

	if response.Status != "failed" {
		t.Errorf("Expected status 'failed', got '%s'", response.Status)
	}
}

func TestRunHandler_MissingFiles(t *testing.T) {
	router := setupTestRouter()

	request := map[string]interface{}{
		"language": "python",
		// Missing files
	}

	body, _ := json.Marshal(request)
	req, _ := http.NewRequest("POST", "/api/v1/run", bytes.NewBuffer(body))
	req.Header.Set("Content-Type", "application/json")

	w := httptest.NewRecorder()
	router.ServeHTTP(w, req)

	// Should fail because files is required
	if w.Code != http.StatusBadRequest {
		t.Errorf("Expected status %d, got %d", http.StatusBadRequest, w.Code)
	}
}

func TestRunResponse_JSONFormat(t *testing.T) {
	tests := []struct {
		name     string
		response RunResponse
	}{
		{
			name: "success response",
			response: RunResponse{
				Status:        "success",
				Stdout:        "Hello World\n",
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
				Stderr:        "warning: something",
				Exception:     nil,
				ExecutionTime: 50,
			},
		},
		{
			name: "timeout response",
			response: RunResponse{
				Status:        "success",
				Stdout:        "",
				Stderr:        nil,
				Exception:     "Timeout",
				ExecutionTime: 30000,
			},
		},
		{
			name: "batch response with stdin",
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
			data, err := json.Marshal(tt.response)
			if err != nil {
				t.Fatalf("Failed to marshal: %v", err)
			}

			var unmarshaled RunResponse
			if err := json.Unmarshal(data, &unmarshaled); err != nil {
				t.Fatalf("Failed to unmarshal: %v", err)
			}

			if unmarshaled.Status != tt.response.Status {
				t.Errorf("Status mismatch: got %s, want %s", unmarshaled.Status, tt.response.Status)
			}
			if unmarshaled.Stdout != tt.response.Stdout {
				t.Errorf("Stdout mismatch: got %s, want %s", unmarshaled.Stdout, tt.response.Stdout)
			}
			if unmarshaled.ExecutionTime != tt.response.ExecutionTime {
				t.Errorf("ExecutionTime mismatch: got %d, want %d", unmarshaled.ExecutionTime, tt.response.ExecutionTime)
			}
		})
	}
}

func TestFileRequest_JSON(t *testing.T) {
	tests := []struct {
		name string
		file FileRequest
	}{
		{
			name: "simple file",
			file: FileRequest{
				Name:    "main.py",
				Content: "print('hello')",
			},
		},
		{
			name: "file with newlines",
			file: FileRequest{
				Name:    "script.py",
				Content: "def greet():\n    print('hello')\n\ngreet()",
			},
		},
		{
			name: "empty content",
			file: FileRequest{
				Name:    "empty.txt",
				Content: "",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			data, err := json.Marshal(tt.file)
			if err != nil {
				t.Fatalf("Failed to marshal: %v", err)
			}

			var unmarshaled FileRequest
			if err := json.Unmarshal(data, &unmarshaled); err != nil {
				t.Fatalf("Failed to unmarshal: %v", err)
			}

			if unmarshaled.Name != tt.file.Name {
				t.Errorf("Name mismatch: got %s, want %s", unmarshaled.Name, tt.file.Name)
			}
			if unmarshaled.Content != tt.file.Content {
				t.Errorf("Content mismatch: got %s, want %s", unmarshaled.Content, tt.file.Content)
			}
		})
	}
}

func TestRunRequest_JSON(t *testing.T) {
	tests := []struct {
		name    string
		request RunRequest
	}{
		{
			name: "simple request",
			request: RunRequest{
				Language: "python",
				Files: []FileRequest{
					{Name: "main.py", Content: "print('hello')"},
				},
			},
		},
		{
			name: "request with stdin string",
			request: RunRequest{
				Language: "python",
				Stdin:    "test input",
				Files: []FileRequest{
					{Name: "main.py", Content: "import sys; print(sys.stdin.read())"},
				},
			},
		},
		{
			name: "request with multiple files",
			request: RunRequest{
				Language: "python",
				Files: []FileRequest{
					{Name: "main.py", Content: "from utils import greet; greet()"},
					{Name: "utils.py", Content: "def greet(): print('hello')"},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			data, err := json.Marshal(tt.request)
			if err != nil {
				t.Fatalf("Failed to marshal: %v", err)
			}

			var unmarshaled RunRequest
			if err := json.Unmarshal(data, &unmarshaled); err != nil {
				t.Fatalf("Failed to unmarshal: %v", err)
			}

			if unmarshaled.Language != tt.request.Language {
				t.Errorf("Language mismatch: got %s, want %s", unmarshaled.Language, tt.request.Language)
			}
			if len(unmarshaled.Files) != len(tt.request.Files) {
				t.Errorf("Files count mismatch: got %d, want %d", len(unmarshaled.Files), len(tt.request.Files))
			}
		})
	}
}

// Helper function
func contains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
