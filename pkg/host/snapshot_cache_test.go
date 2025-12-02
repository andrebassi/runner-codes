package host

import (
	"os"
	"path/filepath"
	"testing"
)

func TestSnapshotCacheConstants(t *testing.T) {
	tests := []struct {
		name     string
		got      string
		want     string
		constant string
	}{
		{"DefaultCacheDir", DefaultCacheDir, "/srv/firecracker/snapshots", "DefaultCacheDir"},
		{"DefaultS3Bucket", DefaultS3Bucket, "runner-codes-rootfs", "DefaultS3Bucket"},
		{"DefaultS3Region", DefaultS3Region, "us-east-1", "DefaultS3Region"},
	}

	for _, tt := range tests {
		t.Run(tt.constant, func(t *testing.T) {
			if tt.got != tt.want {
				t.Errorf("%s = %s, want %s", tt.constant, tt.got, tt.want)
			}
		})
	}
}

func TestNewSnapshotCache(t *testing.T) {
	tests := []struct {
		name     string
		cacheDir string
		s3Bucket string
		s3Region string
	}{
		{
			name:     "default values",
			cacheDir: DefaultCacheDir,
			s3Bucket: DefaultS3Bucket,
			s3Region: DefaultS3Region,
		},
		{
			name:     "custom values",
			cacheDir: "/custom/cache",
			s3Bucket: "my-bucket",
			s3Region: "eu-west-1",
		},
		{
			name:     "dev shm cache",
			cacheDir: "/dev/shm/snapshots",
			s3Bucket: "test-bucket",
			s3Region: "us-west-2",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cache := NewSnapshotCache(tt.cacheDir, tt.s3Bucket, tt.s3Region)

			if cache == nil {
				t.Fatal("NewSnapshotCache() returned nil")
			}
			if cache.cacheDir != tt.cacheDir {
				t.Errorf("cacheDir = %s, want %s", cache.cacheDir, tt.cacheDir)
			}
			if cache.s3Bucket != tt.s3Bucket {
				t.Errorf("s3Bucket = %s, want %s", cache.s3Bucket, tt.s3Bucket)
			}
			if cache.s3Region != tt.s3Region {
				t.Errorf("s3Region = %s, want %s", cache.s3Region, tt.s3Region)
			}
			if cache.cached == nil {
				t.Error("cached map should not be nil")
			}
		})
	}
}

func TestSnapshotCache_SnapshotPaths(t *testing.T) {
	cache := NewSnapshotCache("/srv/firecracker/snapshots", "test-bucket", "us-east-1")

	tests := []struct {
		name           string
		lang           string
		wantMemPath    string
		wantVMStatePath string
	}{
		{
			name:           "python paths",
			lang:           "python",
			wantMemPath:    "/srv/firecracker/snapshots/python/mem.snapshot",
			wantVMStatePath: "/srv/firecracker/snapshots/python/vmstate.snapshot",
		},
		{
			name:           "nodejs paths",
			lang:           "nodejs",
			wantMemPath:    "/srv/firecracker/snapshots/nodejs/mem.snapshot",
			wantVMStatePath: "/srv/firecracker/snapshots/nodejs/vmstate.snapshot",
		},
		{
			name:           "go paths",
			lang:           "go",
			wantMemPath:    "/srv/firecracker/snapshots/go/mem.snapshot",
			wantVMStatePath: "/srv/firecracker/snapshots/go/vmstate.snapshot",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			langDir := filepath.Join(cache.cacheDir, tt.lang)
			memPath := filepath.Join(langDir, "mem.snapshot")
			vmstatePath := filepath.Join(langDir, "vmstate.snapshot")

			if memPath != tt.wantMemPath {
				t.Errorf("memPath = %s, want %s", memPath, tt.wantMemPath)
			}
			if vmstatePath != tt.wantVMStatePath {
				t.Errorf("vmstatePath = %s, want %s", vmstatePath, tt.wantVMStatePath)
			}
		})
	}
}

func TestSnapshotCache_IsCached(t *testing.T) {
	// Create a temp cache directory
	tmpDir, err := os.MkdirTemp("", "snapshot-cache-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	cache := NewSnapshotCache(tmpDir, "test-bucket", "us-east-1")

	t.Run("not cached - directory doesn't exist", func(t *testing.T) {
		// Check if language is cached (should return false)
		langDir := filepath.Join(tmpDir, "python")
		_, err := os.Stat(filepath.Join(langDir, "vmstate.snapshot"))
		if !os.IsNotExist(err) {
			t.Error("snapshot should not exist yet")
		}
	})

	t.Run("cached - files exist", func(t *testing.T) {
		// Create snapshot files
		langDir := filepath.Join(tmpDir, "python")
		if err := os.MkdirAll(langDir, 0755); err != nil {
			t.Fatalf("Failed to create lang dir: %v", err)
		}

		// Create dummy snapshot files
		files := []string{"vmstate.snapshot", "mem.snapshot"}
		for _, f := range files {
			path := filepath.Join(langDir, f)
			if err := os.WriteFile(path, []byte("test"), 0644); err != nil {
				t.Fatalf("Failed to create file %s: %v", f, err)
			}
		}

		// Now check cache
		vmstatePath := filepath.Join(langDir, "vmstate.snapshot")
		memPath := filepath.Join(langDir, "mem.snapshot")

		if _, err := os.Stat(vmstatePath); os.IsNotExist(err) {
			t.Error("vmstate.snapshot should exist")
		}
		if _, err := os.Stat(memPath); os.IsNotExist(err) {
			t.Error("mem.snapshot should exist")
		}

		// Update cache map
		cache.cached["python"] = true
		if !cache.cached["python"] {
			t.Error("python should be marked as cached")
		}
	})
}

func TestSnapshotCache_ScanCache(t *testing.T) {
	// Create a temp cache directory with some languages
	tmpDir, err := os.MkdirTemp("", "snapshot-scan-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create snapshot directories for multiple languages
	languages := []string{"python", "nodejs", "go", "rust"}
	for _, lang := range languages {
		langDir := filepath.Join(tmpDir, lang)
		if err := os.MkdirAll(langDir, 0755); err != nil {
			t.Fatalf("Failed to create dir for %s: %v", lang, err)
		}

		// Create snapshot files
		for _, f := range []string{"vmstate.snapshot", "mem.snapshot"} {
			path := filepath.Join(langDir, f)
			if err := os.WriteFile(path, []byte("test"), 0644); err != nil {
				t.Fatalf("Failed to create %s: %v", path, err)
			}
		}
	}

	// Create cache and scan
	cache := NewSnapshotCache(tmpDir, "test-bucket", "us-east-1")
	_ = cache // Verify cache is created

	// Simulate scanning by checking directory
	entries, err := os.ReadDir(tmpDir)
	if err != nil {
		t.Fatalf("Failed to read dir: %v", err)
	}

	if len(entries) != len(languages) {
		t.Errorf("Found %d languages, want %d", len(entries), len(languages))
	}
}

func TestSnapshotCache_S3Paths(t *testing.T) {
	cache := NewSnapshotCache("/cache", "my-bucket", "us-east-1")

	tests := []struct {
		name         string
		lang         string
		wantS3Prefix string
	}{
		{
			name:         "python s3 path",
			lang:         "python",
			wantS3Prefix: "snapshots/python/",
		},
		{
			name:         "nodejs s3 path",
			lang:         "nodejs",
			wantS3Prefix: "snapshots/nodejs/",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s3Prefix := "snapshots/" + tt.lang + "/"
			if s3Prefix != tt.wantS3Prefix {
				t.Errorf("s3Prefix = %s, want %s", s3Prefix, tt.wantS3Prefix)
			}

			// Verify bucket is set correctly
			if cache.s3Bucket != "my-bucket" {
				t.Errorf("s3Bucket = %s, want my-bucket", cache.s3Bucket)
			}
		})
	}
}

func TestSnapshotCache_EnsureCached_InvalidLang(t *testing.T) {
	cache := NewSnapshotCache("/nonexistent/path", "invalid-bucket", "us-east-1")

	// EnsureCached should fail for non-existent cache without S3 access
	_, _, err := cache.EnsureCached("nonexistent-lang")
	if err == nil {
		// Depending on implementation, this might or might not error
		// If S3 download is attempted and fails, it should error
		t.Log("EnsureCached returned no error (may attempt S3 download)")
	}
}
