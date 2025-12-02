package s3

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/service/s3"
)

// Client wraps S3 operations
type Client struct {
	client *s3.Client
	bucket string
}

// NewClient creates a new S3 client
func NewClient(bucket, region string) (*Client, error) {
	cfg, err := config.LoadDefaultConfig(context.Background(),
		config.WithRegion(region),
	)
	if err != nil {
		return nil, fmt.Errorf("failed to load AWS config: %w", err)
	}

	return &Client{
		client: s3.NewFromConfig(cfg),
		bucket: bucket,
	}, nil
}

// UploadFile uploads a file to S3
func (c *Client) UploadFile(ctx context.Context, localPath, s3Key string) error {
	file, err := os.Open(localPath)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	stat, err := file.Stat()
	if err != nil {
		return fmt.Errorf("failed to stat file: %w", err)
	}

	_, err = c.client.PutObject(ctx, &s3.PutObjectInput{
		Bucket:        aws.String(c.bucket),
		Key:           aws.String(s3Key),
		Body:          file,
		ContentLength: aws.Int64(stat.Size()),
	})
	if err != nil {
		return fmt.Errorf("failed to upload to S3: %w", err)
	}

	return nil
}

// DownloadFile downloads a file from S3
func (c *Client) DownloadFile(ctx context.Context, s3Key, localPath string) error {
	// Ensure directory exists
	dir := filepath.Dir(localPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	result, err := c.client.GetObject(ctx, &s3.GetObjectInput{
		Bucket: aws.String(c.bucket),
		Key:    aws.String(s3Key),
	})
	if err != nil {
		return fmt.Errorf("failed to get object from S3: %w", err)
	}
	defer result.Body.Close()

	file, err := os.Create(localPath)
	if err != nil {
		return fmt.Errorf("failed to create file: %w", err)
	}
	defer file.Close()

	_, err = io.Copy(file, result.Body)
	if err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	return nil
}

// ListObjects lists objects in S3 with a prefix
func (c *Client) ListObjects(ctx context.Context, prefix string) ([]ObjectInfo, error) {
	result, err := c.client.ListObjectsV2(ctx, &s3.ListObjectsV2Input{
		Bucket: aws.String(c.bucket),
		Prefix: aws.String(prefix),
	})
	if err != nil {
		return nil, fmt.Errorf("failed to list objects: %w", err)
	}

	var objects []ObjectInfo
	for _, obj := range result.Contents {
		objects = append(objects, ObjectInfo{
			Key:          aws.ToString(obj.Key),
			Size:         aws.ToInt64(obj.Size),
			LastModified: aws.ToTime(obj.LastModified),
		})
	}

	return objects, nil
}

// ObjectExists checks if an object exists in S3
func (c *Client) ObjectExists(ctx context.Context, key string) (bool, error) {
	_, err := c.client.HeadObject(ctx, &s3.HeadObjectInput{
		Bucket: aws.String(c.bucket),
		Key:    aws.String(key),
	})
	if err != nil {
		return false, nil
	}
	return true, nil
}

// ObjectInfo contains S3 object metadata
type ObjectInfo struct {
	Key          string
	Size         int64
	LastModified time.Time
}

// UploadRootfs uploads a rootfs image to S3
func (c *Client) UploadRootfs(ctx context.Context, lang, localPath string) error {
	key := fmt.Sprintf("rootfs-%s.ext4", lang)
	return c.UploadFile(ctx, localPath, key)
}

// DownloadRootfs downloads a rootfs image from S3
func (c *Client) DownloadRootfs(ctx context.Context, lang, localPath string) error {
	key := fmt.Sprintf("rootfs-%s.ext4", lang)
	return c.DownloadFile(ctx, key, localPath)
}

// UploadSnapshot uploads snapshot files to S3
func (c *Client) UploadSnapshot(ctx context.Context, lang, snapshotDir string) error {
	// Upload vmstate.snapshot
	vmstatePath := filepath.Join(snapshotDir, "vmstate.snapshot")
	vmstateKey := fmt.Sprintf("snapshots/%s/vmstate.snapshot", lang)
	if err := c.UploadFile(ctx, vmstatePath, vmstateKey); err != nil {
		return fmt.Errorf("failed to upload vmstate: %w", err)
	}

	// Upload mem.snapshot
	memPath := filepath.Join(snapshotDir, "mem.snapshot")
	memKey := fmt.Sprintf("snapshots/%s/mem.snapshot", lang)
	if err := c.UploadFile(ctx, memPath, memKey); err != nil {
		return fmt.Errorf("failed to upload mem: %w", err)
	}

	return nil
}

// DownloadSnapshot downloads snapshot files from S3
func (c *Client) DownloadSnapshot(ctx context.Context, lang, localDir string) error {
	// Download vmstate.snapshot
	vmstateKey := fmt.Sprintf("snapshots/%s/vmstate.snapshot", lang)
	vmstatePath := filepath.Join(localDir, "vmstate.snapshot")
	if err := c.DownloadFile(ctx, vmstateKey, vmstatePath); err != nil {
		return fmt.Errorf("failed to download vmstate: %w", err)
	}

	// Download mem.snapshot
	memKey := fmt.Sprintf("snapshots/%s/mem.snapshot", lang)
	memPath := filepath.Join(localDir, "mem.snapshot")
	if err := c.DownloadFile(ctx, memKey, memPath); err != nil {
		return fmt.Errorf("failed to download mem: %w", err)
	}

	return nil
}

// ListRootfs lists all rootfs images in S3
func (c *Client) ListRootfs(ctx context.Context) ([]ObjectInfo, error) {
	return c.ListObjects(ctx, "rootfs-")
}

// ListSnapshots lists all snapshots in S3
func (c *Client) ListSnapshots(ctx context.Context) ([]string, error) {
	objects, err := c.ListObjects(ctx, "snapshots/")
	if err != nil {
		return nil, err
	}

	// Extract unique language names
	langMap := make(map[string]bool)
	for _, obj := range objects {
		// Extract lang from "snapshots/{lang}/..."
		parts := filepath.SplitList(obj.Key)
		if len(parts) >= 2 {
			langMap[parts[1]] = true
		}
	}

	var langs []string
	for lang := range langMap {
		langs = append(langs, lang)
	}

	return langs, nil
}
