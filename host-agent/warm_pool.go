package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"sync"
	"time"
)

const (
	DefaultPoolSize    = 2  // VMs per language
	DefaultMaxIdleTime = 5 * time.Minute
	DefaultVsockCID    = 3
)

// PooledVM represents a pre-warmed VM ready for use
type PooledVM struct {
	InstanceID string
	Lang       string
	APISocket  string
	VsockUDS   string
	Cmd        *exec.Cmd
	Client     *FirecrackerClient
	CreatedAt  time.Time
	InUse      bool
}

// WarmPool manages a pool of pre-warmed VMs for fast code execution
type WarmPool struct {
	mu            sync.Mutex
	cache         *SnapshotCache
	pools         map[string]chan *PooledVM // lang -> channel of ready VMs
	poolSize      int
	maxIdleTime   time.Duration
	stopChan      chan struct{}
	wg            sync.WaitGroup
	priorityLangs []string // languages to keep warm
}

// WarmPoolConfig holds configuration for the warm pool
type WarmPoolConfig struct {
	SnapshotCache *SnapshotCache
	PoolSize      int
	MaxIdleTime   time.Duration
	PriorityLangs []string
}

// NewWarmPool creates a new warm pool
func NewWarmPool(config WarmPoolConfig) *WarmPool {
	if config.PoolSize == 0 {
		config.PoolSize = DefaultPoolSize
	}
	if config.MaxIdleTime == 0 {
		config.MaxIdleTime = DefaultMaxIdleTime
	}

	wp := &WarmPool{
		cache:         config.SnapshotCache,
		pools:         make(map[string]chan *PooledVM),
		poolSize:      config.PoolSize,
		maxIdleTime:   config.MaxIdleTime,
		stopChan:      make(chan struct{}),
		priorityLangs: config.PriorityLangs,
	}

	return wp
}

// Start begins the warm pool management
func (wp *WarmPool) Start() {
	log.Printf("WarmPool: starting with pool size %d per language", wp.poolSize)

	// Pre-warm priority languages
	for _, lang := range wp.priorityLangs {
		wp.ensurePool(lang)
		go wp.fillPool(lang)
	}

	// Start idle cleanup goroutine
	wp.wg.Add(1)
	go wp.idleCleanup()
}

// Stop shuts down the warm pool
func (wp *WarmPool) Stop() {
	close(wp.stopChan)
	wp.wg.Wait()

	// Cleanup all VMs
	wp.mu.Lock()
	defer wp.mu.Unlock()

	for lang, pool := range wp.pools {
		close(pool)
		for vm := range pool {
			wp.destroyVM(vm)
		}
		log.Printf("WarmPool: cleaned up pool for %s", lang)
	}
}

// ensurePool creates a pool channel for a language if it doesn't exist
func (wp *WarmPool) ensurePool(lang string) {
	wp.mu.Lock()
	defer wp.mu.Unlock()

	if _, ok := wp.pools[lang]; !ok {
		wp.pools[lang] = make(chan *PooledVM, wp.poolSize)
		log.Printf("WarmPool: created pool for %s", lang)
	}
}

// GetVM retrieves a warm VM for the language, creating one if needed
func (wp *WarmPool) GetVM(lang string) (*PooledVM, error) {
	wp.ensurePool(lang)

	// Try to get from pool
	wp.mu.Lock()
	pool := wp.pools[lang]
	wp.mu.Unlock()

	select {
	case vm := <-pool:
		vm.InUse = true
		log.Printf("WarmPool: got warm VM %s for %s", vm.InstanceID, lang)
		// Refill pool in background
		go wp.fillPool(lang)
		return vm, nil
	default:
		// Pool empty, create new VM
		log.Printf("WarmPool: pool empty for %s, creating new VM", lang)
		return wp.createVM(lang)
	}
}

// ReturnVM returns a VM to the pool (for reuse) or destroys it
func (wp *WarmPool) ReturnVM(vm *PooledVM, reusable bool) {
	if !reusable || time.Since(vm.CreatedAt) > wp.maxIdleTime {
		wp.destroyVM(vm)
		go wp.fillPool(vm.Lang)
		return
	}

	vm.InUse = false

	wp.mu.Lock()
	pool := wp.pools[vm.Lang]
	wp.mu.Unlock()

	select {
	case pool <- vm:
		log.Printf("WarmPool: returned VM %s to pool", vm.InstanceID)
	default:
		// Pool full, destroy VM
		wp.destroyVM(vm)
	}
}

// createVM creates a new VM from snapshot
func (wp *WarmPool) createVM(lang string) (*PooledVM, error) {
	// Ensure snapshot is cached
	memPath, vmstatePath, err := wp.cache.EnsureCached(lang)
	if err != nil {
		return nil, fmt.Errorf("ensure cached: %w", err)
	}

	// Generate unique instance ID
	instanceID := fmt.Sprintf("%s-%d", lang, time.Now().UnixNano())
	apiSocket := fmt.Sprintf("/tmp/fc-%s.sock", instanceID)
	vsockUDS := fmt.Sprintf("/tmp/fc-%s.vsock", instanceID)

	// Start Firecracker
	cmd, err := StartFirecracker(apiSocket, instanceID)
	if err != nil {
		return nil, fmt.Errorf("start firecracker: %w", err)
	}

	// Create client
	client := NewFirecrackerClient(apiSocket)

	// Load snapshot
	if err := client.LoadSnapshot(vmstatePath, memPath); err != nil {
		cmd.Process.Kill()
		os.Remove(apiSocket)
		return nil, fmt.Errorf("load snapshot: %w", err)
	}

	vm := &PooledVM{
		InstanceID: instanceID,
		Lang:       lang,
		APISocket:  apiSocket,
		VsockUDS:   vsockUDS,
		Cmd:        cmd,
		Client:     client,
		CreatedAt:  time.Now(),
		InUse:      false,
	}

	log.Printf("WarmPool: created VM %s from snapshot", instanceID)
	return vm, nil
}

// destroyVM terminates and cleans up a VM
func (wp *WarmPool) destroyVM(vm *PooledVM) {
	if vm.Client != nil {
		vm.Client.StopInstance()
	}
	if vm.Cmd != nil && vm.Cmd.Process != nil {
		vm.Cmd.Process.Kill()
		vm.Cmd.Wait()
	}
	os.Remove(vm.APISocket)
	os.Remove(vm.VsockUDS)
	log.Printf("WarmPool: destroyed VM %s", vm.InstanceID)
}

// fillPool fills the pool for a language up to poolSize
func (wp *WarmPool) fillPool(lang string) {
	wp.mu.Lock()
	pool, ok := wp.pools[lang]
	wp.mu.Unlock()

	if !ok {
		return
	}

	for {
		select {
		case <-wp.stopChan:
			return
		default:
		}

		// Check if pool is full
		if len(pool) >= wp.poolSize {
			return
		}

		// Create new VM
		vm, err := wp.createVM(lang)
		if err != nil {
			log.Printf("WarmPool: failed to create VM for %s: %v", lang, err)
			return
		}

		select {
		case pool <- vm:
			// Added to pool
		default:
			// Pool filled while we were creating, destroy this VM
			wp.destroyVM(vm)
			return
		}
	}
}

// idleCleanup periodically removes idle VMs that are too old
func (wp *WarmPool) idleCleanup() {
	defer wp.wg.Done()

	ticker := time.NewTicker(1 * time.Minute)
	defer ticker.Stop()

	for {
		select {
		case <-wp.stopChan:
			return
		case <-ticker.C:
			wp.cleanupIdleVMs()
		}
	}
}

// cleanupIdleVMs removes VMs that have been idle too long
func (wp *WarmPool) cleanupIdleVMs() {
	wp.mu.Lock()
	defer wp.mu.Unlock()

	for lang, pool := range wp.pools {
		// Drain and filter
		var toKeep []*PooledVM
		var toDestroy []*PooledVM

	drain:
		for {
			select {
			case vm := <-pool:
				if time.Since(vm.CreatedAt) > wp.maxIdleTime && !vm.InUse {
					toDestroy = append(toDestroy, vm)
				} else {
					toKeep = append(toKeep, vm)
				}
			default:
				break drain
			}
		}

		// Put back the ones to keep
		for _, vm := range toKeep {
			select {
			case pool <- vm:
			default:
				toDestroy = append(toDestroy, vm)
			}
		}

		// Destroy old VMs
		for _, vm := range toDestroy {
			wp.destroyVM(vm)
		}

		if len(toDestroy) > 0 {
			log.Printf("WarmPool: cleaned up %d idle VMs for %s", len(toDestroy), lang)
		}
	}
}

// Stats returns pool statistics
func (wp *WarmPool) Stats() map[string]int {
	wp.mu.Lock()
	defer wp.mu.Unlock()

	stats := make(map[string]int)
	for lang, pool := range wp.pools {
		stats[lang] = len(pool)
	}
	return stats
}

// PrewarmLanguage pre-warms VMs for a specific language
func (wp *WarmPool) PrewarmLanguage(lang string) {
	wp.ensurePool(lang)
	go wp.fillPool(lang)
}

// PrewarmLanguages pre-warms VMs for multiple languages
func (wp *WarmPool) PrewarmLanguages(langs []string) {
	for _, lang := range langs {
		wp.PrewarmLanguage(lang)
	}
}
