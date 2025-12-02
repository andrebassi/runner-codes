package host

import (
	"encoding/json"
	"testing"
)

func TestRunRequest_JSON(t *testing.T) {
	tests := []struct {
		name     string
		request  RunRequest
		wantJSON string
	}{
		{
			name: "basic request",
			request: RunRequest{
				TraceID: "tr-123",
				Lang:    "python",
				Code:    "print('hello')",
				Timeout: 10,
			},
			wantJSON: `{"trace_id":"tr-123","lang":"python","code":"print('hello')","timeout":10}`,
		},
		{
			name: "empty trace_id",
			request: RunRequest{
				Lang:    "nodejs",
				Code:    "console.log('hi')",
				Timeout: 30,
			},
			wantJSON: `{"trace_id":"","lang":"nodejs","code":"console.log('hi')","timeout":30}`,
		},
		{
			name: "zero timeout",
			request: RunRequest{
				TraceID: "tr-456",
				Lang:    "go",
				Code:    "package main",
				Timeout: 0,
			},
			wantJSON: `{"trace_id":"tr-456","lang":"go","code":"package main","timeout":0}`,
		},
		{
			name: "with stdin string",
			request: RunRequest{
				TraceID: "tr-stdin",
				Lang:    "python",
				Code:    "import sys; print(sys.stdin.read())",
				Stdin:   "Hello World",
				Timeout: 10,
			},
			wantJSON: `{"trace_id":"tr-stdin","lang":"python","code":"import sys; print(sys.stdin.read())","stdin":"Hello World","timeout":10}`,
		},
		{
			name: "with files array",
			request: RunRequest{
				TraceID: "tr-files",
				Lang:    "python",
				Code:    "",
				Files: []File{
					{Name: "main.py", Content: "print('hello')"},
				},
				Timeout: 10,
			},
			wantJSON: `{"trace_id":"tr-files","lang":"python","code":"","files":[{"name":"main.py","content":"print('hello')"}],"timeout":10}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Test marshaling
			got, err := json.Marshal(tt.request)
			if err != nil {
				t.Fatalf("Marshal() error = %v", err)
			}

			if string(got) != tt.wantJSON {
				t.Errorf("Marshal() = %s, want %s", got, tt.wantJSON)
			}

			// Test unmarshaling
			var unmarshaled RunRequest
			if err := json.Unmarshal([]byte(tt.wantJSON), &unmarshaled); err != nil {
				t.Fatalf("Unmarshal() error = %v", err)
			}

			// Compare basic fields (can't use == for structs with maps/interfaces)
			if unmarshaled.TraceID != tt.request.TraceID {
				t.Errorf("TraceID = %s, want %s", unmarshaled.TraceID, tt.request.TraceID)
			}
			if unmarshaled.Lang != tt.request.Lang {
				t.Errorf("Lang = %s, want %s", unmarshaled.Lang, tt.request.Lang)
			}
			if unmarshaled.Code != tt.request.Code {
				t.Errorf("Code = %s, want %s", unmarshaled.Code, tt.request.Code)
			}
			if unmarshaled.Timeout != tt.request.Timeout {
				t.Errorf("Timeout = %d, want %d", unmarshaled.Timeout, tt.request.Timeout)
			}
			if len(unmarshaled.Files) != len(tt.request.Files) {
				t.Errorf("Files count = %d, want %d", len(unmarshaled.Files), len(tt.request.Files))
			}
		})
	}
}

func TestFile_JSON(t *testing.T) {
	file := File{
		Name:    "main.py",
		Content: "print('hello')",
	}

	data, err := json.Marshal(file)
	if err != nil {
		t.Fatalf("Marshal() error = %v", err)
	}

	want := `{"name":"main.py","content":"print('hello')"}`
	if string(data) != want {
		t.Errorf("Marshal() = %s, want %s", data, want)
	}

	var unmarshaled File
	if err := json.Unmarshal(data, &unmarshaled); err != nil {
		t.Fatalf("Unmarshal() error = %v", err)
	}

	if unmarshaled != file {
		t.Errorf("Unmarshal() = %+v, want %+v", unmarshaled, file)
	}
}

func TestRunResult_JSON(t *testing.T) {
	tests := []struct {
		name     string
		result   RunResult
		wantJSON string
	}{
		{
			name: "successful result",
			result: RunResult{
				TraceID:       "tr-123",
				Status:        "success",
				Stdout:        "Hello World\n",
				Stderr:        nil,
				Exception:     nil,
				ExecutionTime: 42,
				ExitCode:      0,
			},
			wantJSON: `{"trace_id":"tr-123","status":"success","stdout":"Hello World\n","stderr":null,"exception":null,"executionTime":42}`,
		},
		{
			name: "error result",
			result: RunResult{
				TraceID:       "tr-456",
				Status:        "success",
				Stdout:        "",
				Stderr:        "Error: undefined variable",
				Exception:     nil,
				ExecutionTime: 15,
				ExitCode:      1,
				Error:         "execution failed",
			},
			wantJSON: `{"trace_id":"tr-456","status":"success","stdout":"","stderr":"Error: undefined variable","exception":null,"executionTime":15,"exit_code":1,"error":"execution failed"}`,
		},
		{
			name: "timeout result",
			result: RunResult{
				TraceID:       "tr-789",
				Status:        "success",
				Stdout:        "partial output",
				Stderr:        "Execution timed out",
				Exception:     "Timeout",
				ExecutionTime: 30000,
				ExitCode:      124,
				Error:         "timeout",
			},
			wantJSON: `{"trace_id":"tr-789","status":"success","stdout":"partial output","stderr":"Execution timed out","exception":"Timeout","executionTime":30000,"exit_code":124,"error":"timeout"}`,
		},
		{
			name: "batch result with stdin echo",
			result: RunResult{
				Status:        "success",
				Stdout:        "Hello Peter\n",
				Stderr:        nil,
				Exception:     nil,
				ExecutionTime: 30,
				Stdin:         "Peter",
			},
			wantJSON: `{"status":"success","stdout":"Hello Peter\n","stderr":null,"exception":null,"executionTime":30,"stdin":"Peter"}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := json.Marshal(tt.result)
			if err != nil {
				t.Fatalf("Marshal() error = %v", err)
			}

			if string(got) != tt.wantJSON {
				t.Errorf("Marshal() = %s, want %s", got, tt.wantJSON)
			}

			var unmarshaled RunResult
			if err := json.Unmarshal([]byte(tt.wantJSON), &unmarshaled); err != nil {
				t.Fatalf("Unmarshal() error = %v", err)
			}

			// Compare individual fields since we can't use == with interface{} fields
			if unmarshaled.TraceID != tt.result.TraceID {
				t.Errorf("TraceID = %s, want %s", unmarshaled.TraceID, tt.result.TraceID)
			}
			if unmarshaled.Status != tt.result.Status {
				t.Errorf("Status = %s, want %s", unmarshaled.Status, tt.result.Status)
			}
			if unmarshaled.Stdout != tt.result.Stdout {
				t.Errorf("Stdout = %s, want %s", unmarshaled.Stdout, tt.result.Stdout)
			}
			if unmarshaled.ExecutionTime != tt.result.ExecutionTime {
				t.Errorf("ExecutionTime = %d, want %d", unmarshaled.ExecutionTime, tt.result.ExecutionTime)
			}
			if unmarshaled.ExitCode != tt.result.ExitCode {
				t.Errorf("ExitCode = %d, want %d", unmarshaled.ExitCode, tt.result.ExitCode)
			}
			if unmarshaled.Stdin != tt.result.Stdin {
				t.Errorf("Stdin = %s, want %s", unmarshaled.Stdin, tt.result.Stdin)
			}
		})
	}
}

func TestFirecrackerConfig_Defaults(t *testing.T) {
	config := FirecrackerConfig{
		InstanceID: "test-123",
		KernelPath: "/srv/firecracker/vmlinux",
		RootfsPath: "/srv/firecracker/rootfs.ext4",
		VCPUs:      1,
		MemMiB:     512,
		APISocket:  "/tmp/fc-test.sock",
	}

	if config.InstanceID != "test-123" {
		t.Errorf("InstanceID = %s, want test-123", config.InstanceID)
	}
	if config.VCPUs != 1 {
		t.Errorf("VCPUs = %d, want 1", config.VCPUs)
	}
	if config.MemMiB != 512 {
		t.Errorf("MemMiB = %d, want 512", config.MemMiB)
	}
}

func TestMachineConfig_JSON(t *testing.T) {
	config := MachineConfig{
		VCPUCount:  2,
		MemSizeMiB: 1024,
		SMT:        true,
	}

	data, err := json.Marshal(config)
	if err != nil {
		t.Fatalf("Marshal() error = %v", err)
	}

	want := `{"vcpu_count":2,"mem_size_mib":1024,"smt":true}`
	if string(data) != want {
		t.Errorf("Marshal() = %s, want %s", data, want)
	}
}

func TestBootSource_JSON(t *testing.T) {
	boot := BootSource{
		KernelImagePath: "/srv/firecracker/vmlinux",
		BootArgs:        "console=ttyS0 reboot=k panic=1",
	}

	data, err := json.Marshal(boot)
	if err != nil {
		t.Fatalf("Marshal() error = %v", err)
	}

	want := `{"kernel_image_path":"/srv/firecracker/vmlinux","boot_args":"console=ttyS0 reboot=k panic=1"}`
	if string(data) != want {
		t.Errorf("Marshal() = %s, want %s", data, want)
	}
}

func TestDrive_JSON(t *testing.T) {
	drive := Drive{
		DriveID:      "rootfs",
		PathOnHost:   "/srv/firecracker/rootfs.ext4",
		IsRootDevice: true,
		IsReadOnly:   false,
	}

	data, err := json.Marshal(drive)
	if err != nil {
		t.Fatalf("Marshal() error = %v", err)
	}

	want := `{"drive_id":"rootfs","path_on_host":"/srv/firecracker/rootfs.ext4","is_root_device":true,"is_read_only":false}`
	if string(data) != want {
		t.Errorf("Marshal() = %s, want %s", data, want)
	}
}

func TestVsockConfig_JSON(t *testing.T) {
	vsock := VsockConfig{
		GuestCID: 3,
		UDSPath:  "/tmp/fc-test.vsock",
	}

	data, err := json.Marshal(vsock)
	if err != nil {
		t.Fatalf("Marshal() error = %v", err)
	}

	want := `{"guest_cid":3,"uds_path":"/tmp/fc-test.vsock"}`
	if string(data) != want {
		t.Errorf("Marshal() = %s, want %s", data, want)
	}
}

func TestAction_JSON(t *testing.T) {
	tests := []struct {
		name       string
		actionType string
		wantJSON   string
	}{
		{
			name:       "InstanceStart",
			actionType: "InstanceStart",
			wantJSON:   `{"action_type":"InstanceStart"}`,
		},
		{
			name:       "SendCtrlAltDel",
			actionType: "SendCtrlAltDel",
			wantJSON:   `{"action_type":"SendCtrlAltDel"}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			action := Action{ActionType: tt.actionType}
			data, err := json.Marshal(action)
			if err != nil {
				t.Fatalf("Marshal() error = %v", err)
			}

			if string(data) != tt.wantJSON {
				t.Errorf("Marshal() = %s, want %s", data, tt.wantJSON)
			}
		})
	}
}
