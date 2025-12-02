package api

import (
	"context"
	"fmt"
	"net/http"
	"time"

	"github.com/gin-gonic/gin"
	"infra-operator/internal/config"
	"infra-operator/internal/rootfs"
	"infra-operator/internal/s3"
	"infra-operator/internal/snapshot"
)

// Note: config is still used for Config type and path methods

// Server is the API server
type Server struct {
	cfg      *config.Config
	router   *gin.Engine
	s3Client *s3.Client
	snapshot *snapshot.Manager
	rootfs   *rootfs.Builder
}

// NewServer creates a new API server
func NewServer(cfg *config.Config) (*Server, error) {
	s3Client, err := s3.NewClient(cfg.S3Bucket, cfg.S3Region)
	if err != nil {
		return nil, fmt.Errorf("failed to create S3 client: %w", err)
	}

	gin.SetMode(gin.ReleaseMode)
	router := gin.New()
	router.Use(gin.Recovery())
	router.Use(gin.Logger())

	srv := &Server{
		cfg:      cfg,
		router:   router,
		s3Client: s3Client,
		snapshot: snapshot.NewManager(cfg),
		rootfs:   rootfs.NewBuilder(cfg),
	}

	srv.setupRoutes()
	return srv, nil
}

func (s *Server) setupRoutes() {
	// Health check
	s.router.GET("/health", s.healthHandler)

	// API v1
	v1 := s.router.Group("/api/v1")
	{
		// Rootfs
		v1.GET("/rootfs", s.listRootfsHandler)
		v1.POST("/rootfs/:lang/upload", s.uploadRootfsHandler)
		v1.POST("/rootfs/:lang/download", s.downloadRootfsHandler)

		// Snapshots
		v1.GET("/snapshots", s.listSnapshotsHandler)
		v1.POST("/snapshots/:lang/upload", s.uploadSnapshotHandler)
		v1.POST("/snapshots/:lang/download", s.downloadSnapshotHandler)

		// Execute - our native format
		v1.POST("/execute", s.executeHandler)

		// Run - Simplified execution endpoint
		v1.POST("/run", s.runHandler)
	}
}

// Run starts the server
func (s *Server) Run(host string, port int) error {
	addr := fmt.Sprintf("%s:%d", host, port)
	return s.router.Run(addr)
}

// ========================
// HANDLERS
// ========================

func (s *Server) healthHandler(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":  "healthy",
		"version": "0.1.0",
	})
}

func (s *Server) listRootfsHandler(c *gin.Context) {
	remote := c.Query("remote") == "true"

	if remote {
		objects, err := s.s3Client.ListRootfs(c.Request.Context())
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		items := make([]gin.H, 0)
		for _, obj := range objects {
			items = append(items, gin.H{
				"key":          obj.Key,
				"size":         obj.Size,
				"lastModified": obj.LastModified,
			})
		}
		c.JSON(http.StatusOK, gin.H{"rootfs": items, "source": "s3"})
	} else {
		images, err := s.rootfs.List()
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		items := make([]gin.H, 0)
		for _, img := range images {
			items = append(items, gin.H{
				"language": img.Language,
				"path":     img.Path,
				"size":     img.Size,
			})
		}
		c.JSON(http.StatusOK, gin.H{"rootfs": items, "source": "local"})
	}
}

func (s *Server) uploadRootfsHandler(c *gin.Context) {
	lang := c.Param("lang")
	localPath := s.cfg.RootfsPath(lang)
	if err := s.s3Client.UploadRootfs(c.Request.Context(), lang, localPath); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "rootfs uploaded",
		"s3_path": fmt.Sprintf("s3://%s/rootfs-%s.ext4", s.cfg.S3Bucket, lang),
	})
}

func (s *Server) downloadRootfsHandler(c *gin.Context) {
	lang := c.Param("lang")
	localPath := s.cfg.RootfsPath(lang)
	if err := s.s3Client.DownloadRootfs(c.Request.Context(), lang, localPath); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message":    "rootfs downloaded",
		"local_path": localPath,
	})
}

func (s *Server) listSnapshotsHandler(c *gin.Context) {
	remote := c.Query("remote") == "true"

	if remote {
		objects, err := s.s3Client.ListObjects(c.Request.Context(), "snapshots/")
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		// Group by language
		langMap := make(map[string]gin.H)
		for _, obj := range objects {
			// Parse s3://bucket/snapshots/{lang}/vmstate.snapshot
			parts := splitPath(obj.Key)
			if len(parts) >= 2 {
				lang := parts[1]
				if _, exists := langMap[lang]; !exists {
					langMap[lang] = gin.H{
						"language": lang,
						"s3_path":  fmt.Sprintf("s3://%s/snapshots/%s/", s.cfg.S3Bucket, lang),
					}
				}
			}
		}

		items := make([]gin.H, 0)
		for _, v := range langMap {
			items = append(items, v)
		}
		c.JSON(http.StatusOK, gin.H{"snapshots": items, "source": "s3"})
	} else {
		snapshots, err := s.snapshot.List()
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		items := make([]gin.H, 0)
		for _, snap := range snapshots {
			items = append(items, gin.H{
				"language":     snap.Language,
				"path":         snap.Path,
				"vmstate_size": snap.VmstateSize,
				"mem_size":     snap.MemSize,
			})
		}
		c.JSON(http.StatusOK, gin.H{"snapshots": items, "source": "local"})
	}
}

func (s *Server) uploadSnapshotHandler(c *gin.Context) {
	lang := c.Param("lang")
	snapshotDir := s.cfg.SnapshotDir(lang)

	if err := s.s3Client.UploadSnapshot(c.Request.Context(), lang, snapshotDir); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "snapshot uploaded",
		"s3_path": fmt.Sprintf("s3://%s/snapshots/%s/", s.cfg.S3Bucket, lang),
	})
}

func (s *Server) downloadSnapshotHandler(c *gin.Context) {
	lang := c.Param("lang")
	localDir := s.cfg.SnapshotDir(lang)

	start := time.Now()
	if err := s.s3Client.DownloadSnapshot(c.Request.Context(), lang, localDir); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	elapsed := time.Since(start)

	c.JSON(http.StatusOK, gin.H{
		"message":       "snapshot downloaded",
		"local_path":    localDir,
		"download_time": elapsed.String(),
	})
}

// FileRequest represents a source file in the request
type FileRequest struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

// RunRequest is the /run API request format
type RunRequest struct {
	Language string        `json:"language" binding:"required"`
	Stdin    interface{}   `json:"stdin,omitempty"` // string or []string for batch
	Files    []FileRequest `json:"files" binding:"required"`
}

// RunResponse is a single execution response
type RunResponse struct {
	Status        string      `json:"status"`        // "success" or "failed"
	Stdout        string      `json:"stdout"`        // Standard output
	Stderr        interface{} `json:"stderr"`        // null if empty
	Exception     interface{} `json:"exception"`     // null if none
	ExecutionTime int64       `json:"executionTime"` // Time in ms
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"` // Echo stdin for batch
}

// runHandler handles POST /api/v1/run
func (s *Server) runHandler(c *gin.Context) {
	var req RunRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, RunResponse{
			Status: "failed",
			Error:  "E004: access_token missing",
		})
		return
	}

	// Check for batch execution (stdin is array)
	if stdinArray, ok := req.Stdin.([]interface{}); ok && len(stdinArray) > 0 {
		s.handleBatchExecution(c, req, stdinArray)
		return
	}

	// Single execution
	response := s.executeCode(req, s.getStdinString(req.Stdin))
	c.JSON(http.StatusOK, response)
}

// handleBatchExecution processes multiple stdin values
func (s *Server) handleBatchExecution(c *gin.Context, req RunRequest, stdinArray []interface{}) {
	results := make([]RunResponse, 0, len(stdinArray))

	for _, stdin := range stdinArray {
		stdinStr := s.getStdinString(stdin)
		response := s.executeCode(req, stdinStr)
		response.Stdin = stdinStr
		results = append(results, response)
	}

	c.JSON(http.StatusOK, results)
}

// executeCode runs a single execution
func (s *Server) executeCode(req RunRequest, stdin string) RunResponse {
	// Validate stdin length (1MB limit)
	if len(stdin) > 1_000_000 {
		return RunResponse{
			Status: "failed",
			Error:  "E005: stdin too long",
		}
	}

	// TODO: Execute via Firecracker microVM + vsock
	// For now, simulate response format
	response := RunResponse{
		Status:        "success",
		Stdout:        "[Execution pending - vsock implementation required]",
		Stderr:        nil,
		Exception:     nil,
		ExecutionTime: 0,
	}

	return response
}

// getStdinString converts interface{} to string
func (s *Server) getStdinString(stdin interface{}) string {
	if stdin == nil {
		return ""
	}
	switch v := stdin.(type) {
	case string:
		return v
	default:
		return fmt.Sprintf("%v", v)
	}
}

// executeHandler handles POST /api/v1/execute (internal format)
func (s *Server) executeHandler(c *gin.Context) {
	var req struct {
		Language string        `json:"language" binding:"required"`
		Code     string        `json:"code"`
		Files    []FileRequest `json:"files,omitempty"`
		Stdin    interface{}   `json:"stdin,omitempty"`
		Timeout  int           `json:"timeout"`
	}

	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, RunResponse{
			Status: "failed",
			Error:  err.Error(),
		})
		return
	}

	// Validate: either code or files must be provided
	if req.Code == "" && len(req.Files) == 0 {
		c.JSON(http.StatusBadRequest, RunResponse{
			Status: "failed",
			Error:  "either 'code' or 'files' must be provided",
		})
		return
	}

	timeout := req.Timeout
	if timeout == 0 {
		timeout = 30
	}

	ctx, cancel := context.WithTimeout(c.Request.Context(), time.Duration(timeout)*time.Second)
	defer cancel()

	// Load snapshot
	start := time.Now()
	fc, err := s.snapshot.Load(ctx, req.Language)
	if err != nil {
		c.JSON(http.StatusInternalServerError, RunResponse{
			Status:    "failed",
			Exception: fmt.Sprintf("failed to load snapshot: %v", err),
		})
		return
	}
	defer fc.Stop()
	loadTime := time.Since(start)

	// TODO: Execute code via vsock
	c.JSON(http.StatusOK, gin.H{
		"status":        "success",
		"stdout":        "[Execution via vsock - pending]",
		"stderr":        nil,
		"exception":     nil,
		"executionTime": 0,
		"loadTime":      loadTime.Milliseconds(),
	})
}

func splitPath(path string) []string {
	var parts []string
	current := ""
	for _, c := range path {
		if c == '/' {
			if current != "" {
				parts = append(parts, current)
				current = ""
			}
		} else {
			current += string(c)
		}
	}
	if current != "" {
		parts = append(parts, current)
	}
	return parts
}
