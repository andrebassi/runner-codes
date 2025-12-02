package main

// RunRequest represents a code execution request sent to the guest
type RunRequest struct {
	TraceID string            `json:"trace_id"`
	Lang    string            `json:"lang"`
	Code    string            `json:"code"`
	Timeout int               `json:"timeout"`
	Meta    map[string]string `json:"meta,omitempty"`
}

// RunResult represents the execution result from the guest
type RunResult struct {
	TraceID  string `json:"trace_id,omitempty"`
	Stdout   string `json:"stdout"`
	Stderr   string `json:"stderr"`
	ExitCode int    `json:"exit_code"`
	Error    string `json:"error,omitempty"`
}

// FirecrackerConfig holds configuration for a Firecracker instance
type FirecrackerConfig struct {
	InstanceID string
	KernelPath string
	RootfsPath string
	VCPUs      int
	MemMiB     int
	BootArgs   string
	APISocket  string
}

// MachineConfig represents Firecracker machine configuration
type MachineConfig struct {
	VCPUCount  int  `json:"vcpu_count"`
	MemSizeMiB int  `json:"mem_size_mib"`
	SMT        bool `json:"smt"` // Renamed from ht_enabled in Firecracker v1.7.0
}

// BootSource represents Firecracker boot source configuration
type BootSource struct {
	KernelImagePath string `json:"kernel_image_path"`
	BootArgs        string `json:"boot_args"`
}

// Drive represents Firecracker drive configuration
type Drive struct {
	DriveID      string `json:"drive_id"`
	PathOnHost   string `json:"path_on_host"`
	IsRootDevice bool   `json:"is_root_device"`
	IsReadOnly   bool   `json:"is_read_only"`
}

// VsockConfig represents Firecracker vsock configuration
type VsockConfig struct {
	GuestCID   uint32 `json:"guest_cid"`
	UDSPath    string `json:"uds_path,omitempty"`
}

// Action represents a Firecracker action
type Action struct {
	ActionType string `json:"action_type"`
}
