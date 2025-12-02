package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"time"
)

const (
	DefaultS3Bucket    = "llm-firesandbox-rootfs"
	DefaultS3Region    = "us-east-1"
	DefaultCacheDir    = "/srv/firecracker/snapshots"
	SnapshotMemFile    = "mem.snapshot"
	SnapshotVMStateFile = "vmstate.snapshot"
)

// SnapshotCache manages local caching of snapshots from S3
type SnapshotCache struct {
	mu          sync.RWMutex
	cacheDir    string
	s3Bucket    string
	s3Region    string
	cached      map[string]bool      // lang -> is cached
	downloading map[string]chan struct{} // lang -> done channel (for concurrent downloads)
}

// NewSnapshotCache creates a new snapshot cache manager
func NewSnapshotCache(cacheDir, s3Bucket, s3Region string) *SnapshotCache {
	if cacheDir == "" {
		cacheDir = DefaultCacheDir
	}
	if s3Bucket == "" {
		s3Bucket = DefaultS3Bucket
	}
	if s3Region == "" {
		s3Region = DefaultS3Region
	}

	sc := &SnapshotCache{
		cacheDir:    cacheDir,
		s3Bucket:    s3Bucket,
		s3Region:    s3Region,
		cached:      make(map[string]bool),
		downloading: make(map[string]chan struct{}),
	}

	// Scan existing cache
	sc.scanCache()

	return sc
}

// scanCache checks which languages are already cached locally
func (sc *SnapshotCache) scanCache() {
	entries, err := os.ReadDir(sc.cacheDir)
	if err != nil {
		log.Printf("SnapshotCache: cache dir not found, will create: %s", sc.cacheDir)
		return
	}

	for _, entry := range entries {
		if entry.IsDir() {
			lang := entry.Name()
			if sc.hasLocalSnapshot(lang) {
				sc.cached[lang] = true
				log.Printf("SnapshotCache: found cached snapshot for %s", lang)
			}
		}
	}

	log.Printf("SnapshotCache: %d languages cached locally", len(sc.cached))
}

// hasLocalSnapshot checks if a language has both snapshot files locally
func (sc *SnapshotCache) hasLocalSnapshot(lang string) bool {
	memPath := filepath.Join(sc.cacheDir, lang, SnapshotMemFile)
	vmstatePath := filepath.Join(sc.cacheDir, lang, SnapshotVMStateFile)

	memInfo, err1 := os.Stat(memPath)
	vmstateInfo, err2 := os.Stat(vmstatePath)

	if err1 != nil || err2 != nil {
		return false
	}

	// Check files have reasonable sizes
	return memInfo.Size() > 0 && vmstateInfo.Size() > 0
}

// IsCached returns true if the language snapshot is cached locally
func (sc *SnapshotCache) IsCached(lang string) bool {
	sc.mu.RLock()
	defer sc.mu.RUnlock()
	return sc.cached[lang]
}

// GetSnapshotPaths returns the local paths for a language's snapshot files
func (sc *SnapshotCache) GetSnapshotPaths(lang string) (memPath, vmstatePath string) {
	memPath = filepath.Join(sc.cacheDir, lang, SnapshotMemFile)
	vmstatePath = filepath.Join(sc.cacheDir, lang, SnapshotVMStateFile)
	return
}

// EnsureCached downloads the snapshot from S3 if not already cached
// Returns the paths to mem.snapshot and vmstate.snapshot
func (sc *SnapshotCache) EnsureCached(lang string) (memPath, vmstatePath string, err error) {
	memPath, vmstatePath = sc.GetSnapshotPaths(lang)

	// Fast path: already cached
	if sc.IsCached(lang) {
		return memPath, vmstatePath, nil
	}

	// Check if another goroutine is downloading
	sc.mu.Lock()
	if done, ok := sc.downloading[lang]; ok {
		sc.mu.Unlock()
		// Wait for the other download to complete
		<-done
		if sc.IsCached(lang) {
			return memPath, vmstatePath, nil
		}
		return "", "", fmt.Errorf("snapshot download failed for %s", lang)
	}

	// Start download
	done := make(chan struct{})
	sc.downloading[lang] = done
	sc.mu.Unlock()

	// Download from S3
	err = sc.downloadFromS3(lang)

	// Cleanup and notify waiters
	sc.mu.Lock()
	delete(sc.downloading, lang)
	if err == nil {
		sc.cached[lang] = true
	}
	sc.mu.Unlock()
	close(done)

	if err != nil {
		return "", "", err
	}

	return memPath, vmstatePath, nil
}

// downloadFromS3 downloads snapshot files from S3
func (sc *SnapshotCache) downloadFromS3(lang string) error {
	langDir := filepath.Join(sc.cacheDir, lang)
	if err := os.MkdirAll(langDir, 0755); err != nil {
		return fmt.Errorf("create cache dir: %w", err)
	}

	s3Prefix := fmt.Sprintf("s3://%s/snapshots/%s/", sc.s3Bucket, lang)
	memPath := filepath.Join(langDir, SnapshotMemFile)
	vmstatePath := filepath.Join(langDir, SnapshotVMStateFile)

	start := time.Now()
	log.Printf("SnapshotCache: downloading %s from S3...", lang)

	// Download mem.snapshot
	memS3 := fmt.Sprintf("%s%s", s3Prefix, SnapshotMemFile)
	if err := sc.s3Download(memS3, memPath); err != nil {
		return fmt.Errorf("download mem.snapshot: %w", err)
	}

	// Download vmstate.snapshot
	vmstateS3 := fmt.Sprintf("%s%s", s3Prefix, SnapshotVMStateFile)
	if err := sc.s3Download(vmstateS3, vmstatePath); err != nil {
		os.Remove(memPath) // cleanup partial download
		return fmt.Errorf("download vmstate.snapshot: %w", err)
	}

	elapsed := time.Since(start)
	log.Printf("SnapshotCache: downloaded %s in %v", lang, elapsed)

	return nil
}

// s3Download downloads a file from S3 using AWS CLI
func (sc *SnapshotCache) s3Download(s3Path, localPath string) error {
	cmd := exec.Command("aws", "s3", "cp", s3Path, localPath,
		"--region", sc.s3Region,
		"--only-show-errors",
	)
	cmd.Env = os.Environ()

	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("aws s3 cp failed: %s: %w", string(output), err)
	}

	return nil
}

// PrewarmLanguages downloads snapshots for the given languages in background
func (sc *SnapshotCache) PrewarmLanguages(languages []string) {
	go func() {
		for _, lang := range languages {
			if !sc.IsCached(lang) {
				_, _, err := sc.EnsureCached(lang)
				if err != nil {
					log.Printf("SnapshotCache: failed to prewarm %s: %v", lang, err)
				}
			}
		}
	}()
}

// GetCachedLanguages returns a list of all cached languages
func (sc *SnapshotCache) GetCachedLanguages() []string {
	sc.mu.RLock()
	defer sc.mu.RUnlock()

	langs := make([]string, 0, len(sc.cached))
	for lang := range sc.cached {
		langs = append(langs, lang)
	}
	return langs
}

// ClearCache removes all cached snapshots
func (sc *SnapshotCache) ClearCache() error {
	sc.mu.Lock()
	defer sc.mu.Unlock()

	if err := os.RemoveAll(sc.cacheDir); err != nil {
		return err
	}
	sc.cached = make(map[string]bool)
	return os.MkdirAll(sc.cacheDir, 0755)
}

// RemoveLanguage removes a specific language from cache
func (sc *SnapshotCache) RemoveLanguage(lang string) error {
	sc.mu.Lock()
	defer sc.mu.Unlock()

	langDir := filepath.Join(sc.cacheDir, lang)
	if err := os.RemoveAll(langDir); err != nil {
		return err
	}
	delete(sc.cached, lang)
	return nil
}
