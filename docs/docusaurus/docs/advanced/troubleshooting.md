---
title: 'Troubleshooting'
description: 'Common issues and their solutions'
---

## Quick Diagnostics

Run these commands to quickly diagnose issues:

```bash title="Check Firecracker process"
ps aux | grep firecracker
```

```bash title="Check KVM availability"
ls -la /dev/kvm
```

```bash title="Check vhost_vsock"
ls -la /dev/vhost-vsock
```

```bash title="Check vsock socket exists"
ls -la /tmp/fc-*.vsock
```

```bash title="View Firecracker logs"
journalctl -u firecracker -f
```

## Common Issues

### VM Won't Start

#### KVM Error

**Error:**

```text title="Error message"
Error creating VM: cannot create KVM instance
```

**Cause:** KVM module not loaded or no access to /dev/kvm

**Solution:**

```bash title="Load KVM module"
sudo modprobe kvm
sudo modprobe kvm_intel  # or kvm_amd
```

```bash title="Check permissions"
ls -la /dev/kvm
```

```bash title="Add user to kvm group"
sudo usermod -aG kvm $USER
```

---

#### Vhost Vsock Error

**Error:**

```text title="Error message"
Error configuring vsock: cannot open /dev/vhost-vsock
```

**Cause:** vhost_vsock module not loaded

**Solution:**

```bash title="Load vhost_vsock module"
sudo modprobe vhost_vsock
```

```bash title="Verify"
ls -la /dev/vhost-vsock
```

```bash title="Make permanent"
echo "vhost_vsock" | sudo tee -a /etc/modules
```

---

#### Missing Kernel

**Error:**

```text title="Error message"
Error: cannot open kernel file: /srv/firecracker/vmlinux
```

**Solution:**

```bash title="Download kernel"
task s3:download
```

```bash title="Verify"
ls -la /srv/firecracker/images/vmlinux
```

---

#### Missing Rootfs

**Error:**

```text title="Error message"
Error: cannot open drive file: /srv/firecracker/rootfs.ext4
```

**Solution:**

```bash title="Download rootfs images"
task s3:download
```

```bash title="Verify"
ls -la /srv/firecracker/images/rootfs-*.ext4
```

---

#### Socket In Use

**Error:**

```text title="Error message"
Error: cannot bind to socket /tmp/fc-12345.sock: Address already in use
```

**Solution:**

```bash title="Find and kill existing Firecracker process"
ps aux | grep firecracker
kill -9 <PID>
```

```bash title="Remove stale sockets"
rm -f /tmp/fc-*.sock /tmp/fc-*.vsock
```

---

### Connection Issues

#### Socket Not Found

**Error:**

```text title="Error message"
dial unix /tmp/fc-12345.vsock: no such file or directory
```

**Causes:**
1. VM not started
2. vsock not configured
3. Wrong socket path

**Solution:**

```bash title="Verify VM is running"
ps aux | grep firecracker
```

```bash title="Check vsock was configured"
curl --unix-socket /tmp/fc-12345.sock \
  http://localhost/vsock 2>/dev/null | jq
```

```bash title="Verify socket exists"
ls -la /tmp/fc-12345.vsock
```

---

#### Connection Refused

**Error:**

```text title="Error message"
NO 5000
```

**Causes:**
1. infra.operator (guest mode) not started
2. Wrong port number
3. VM not fully booted

**Solution:**

```bash title="Wait longer for VM to boot"
sleep 5
```

---

#### Timeout Error

**Error:**

```text title="Error message"
read: deadline exceeded
```

**Causes:**
1. infra.operator (guest mode) crashed
2. Code execution taking too long
3. Deadlock in guest

**Solution:**

```go title="Increase timeout"
job := Job{
    Timeout: 60,  // Increase from default
}
```

```bash title="Check if VM is still running"
curl --unix-socket /tmp/fc.sock http://localhost/
```

---

### Execution Errors

#### Execution Timeout

**Error:**

```json title="Response"
{
  "exit_code": -1,
  "error": "execution timeout"
}
```

**Causes:**
1. Infinite loop in code
2. Timeout too short
3. Heavy computation

**Solution:**

```go title="Increase execution timeout"
job := Job{
    Code:    "heavy_computation()",
    Timeout: 60,  // seconds
}
```

---

#### Out of Memory

**Error:**

```json title="Response"
{
  "exit_code": 137,
  "stderr": "Killed"
}
```

**Cause:** Code exceeded memory limit

**Solution:**

```json title="Increase VM memory"
{
  "vcpu_count": 1,
  "mem_size_mib": 1024
}
```

---

#### Command Not Found

**Error:**

```json title="Response"
{
  "exit_code": 127,
  "stderr": "python3: command not found"
}
```

**Cause:** Wrong rootfs or language not installed

**Solution:**

```bash title="Verify using correct rootfs"
ls -la /srv/firecracker/images/rootfs-python.ext4
```

```bash title="Mount and check contents"
sudo mount -o loop rootfs-python.ext4 /mnt
ls /mnt/usr/bin/python3
sudo umount /mnt
```

---

#### Permission Denied

**Error:**

```json title="Response"
{
  "exit_code": 126,
  "stderr": "permission denied"
}
```

**Cause:** Script file not executable

**Solution:**

```go title="Set executable permission"
err := os.WriteFile(scriptPath, []byte(code), 0755)
```

---

### Build Errors

#### Package Not Found

**Error:**

```text title="Error message"
cannot find package syscall (using -tags vsock)
```

**Solution:**

```bash title="Build with correct tags"
GOOS=linux GOARCH=amd64 CGO_ENABLED=0 \
  go build -o bin/infra.operator-linux ./cmd/infra.operator
```

---

#### Wrong Architecture

**Error:**

```text title="Error message"
cannot execute binary file: Exec format error
```

**Cause:** Wrong architecture

**Solution:**

```bash title="Build for x86_64"
GOOS=linux GOARCH=amd64 go build -o bin/infra.operator-linux ./cmd/infra.operator
```

```bash title="Build for ARM64"
GOOS=linux GOARCH=arm64 go build -o bin/infra.operator-arm64 ./cmd/infra.operator
```

```bash title="Verify binary"
file bin/infra.operator-linux
```

---

### S3 Issues

#### Access Denied

**Error:**

```text title="Error message"
An error occurred (AccessDenied) when calling the GetObject operation
```

**Solution:**

```bash title="Check AWS credentials"
aws sts get-caller-identity
```

```bash title="Verify bucket policy allows access"
aws s3 ls s3://llm-infra-operator-rootfs/
```

```bash title="Check IAM policy"
aws iam get-user-policy --user-name <user> --policy-name <policy>
```

---

#### Bucket Not Found

**Error:**

```text title="Error message"
An error occurred (NoSuchBucket) when calling the ListObjects operation
```

**Solution:**

```bash title="Create bucket"
aws s3 mb s3://llm-infra-operator-rootfs --region us-east-1
```

```bash title="Verify"
aws s3 ls
```

---

## Debug Commands

### Check VM Status

```bash title="Check VM via Firecracker API"
curl --unix-socket /tmp/fc.sock http://localhost/ | jq
curl --unix-socket /tmp/fc.sock http://localhost/machine-config | jq
curl --unix-socket /tmp/fc.sock http://localhost/vsock | jq
```

### Monitor Serial Console

```bash title="Connect to serial console"
screen /dev/pts/X
```

### Test vsock Connection

```bash title="Manual vsock test"
exec 3<>/dev/tcp/localhost/5000 || echo "Failed"
```

```bash title="Test with netcat"
echo -e "CONNECT 5000\n" | nc -U /tmp/fc-12345.vsock
```

### Check Guest Status

```bash title="Send test job via vsock"
{
  echo "CONNECT 5000"
  sleep 0.5
  echo -ne '\x00\x00\x00\x2B{"trace_id":"test","lang":"bash","code":"echo OK","timeout":10}'
} | nc -U /tmp/fc-12345.vsock
```

---

## Log Locations

| Component | Log Location |
|-----------|--------------|
| infra.operator (host) | stdout / systemd journal |
| Firecracker | stderr / serial console |
| infra.operator (guest) | /var/log/infra.operator.log (in VM) |
| System | /var/log/syslog |

### Enable Debug Logging

```go title="Enable debug in infra.operator"
log.SetLevel(log.DebugLevel)
```

### Firecracker Debug Mode

```bash title="Start Firecracker with debug logging"
firecracker --api-sock /tmp/fc.sock \
  --log-path /var/log/firecracker.log \
  --level Debug
```

---

## Health Checks

### Automated Health Check Script

```bash title="Check KVM"
if [ ! -c /dev/kvm ]; then
    echo "FAIL: /dev/kvm not available"
    exit 1
fi
```

```bash title="Check vhost_vsock"
if [ ! -c /dev/vhost-vsock ]; then
    echo "FAIL: /dev/vhost-vsock not available"
    exit 1
fi
```

```bash title="Check kernel"
if [ ! -f /srv/firecracker/images/vmlinux ]; then
    echo "FAIL: kernel not found"
    exit 1
fi
```

```bash title="Check rootfs images"
for lang in python nodejs go rust bash; do
    if [ ! -f /srv/firecracker/images/rootfs-${lang}.ext4 ]; then
        echo "FAIL: rootfs-${lang}.ext4 not found"
        exit 1
    fi
done
```

```bash title="Check disk space"
FREE=$(df /srv -m | tail -1 | awk '{print $4}')
if [ "$FREE" -lt 5000 ]; then
    echo "WARN: Low disk space: ${FREE}MB"
fi
echo "OK: All checks passed"
```

### Run Health Check

```bash title="Run health check"
task aws:healthcheck
```

---

## Quick Fixes Cheatsheet

| Problem | Quick Fix |
|---------|-----------|
| VM won't start | `sudo modprobe kvm kvm_intel vhost_vsock` |
| Socket in use | `rm -f /tmp/fc-*.sock /tmp/fc-*.vsock` |
| vsock refused | Wait longer, check infra.operator service |
| Timeout | Increase timeout, check for loops |
| OOM | Increase `mem_size_mib` |
| Wrong rootfs | Check language → rootfs mapping |
| S3 access denied | Check AWS credentials |
| Binary format | Cross-compile with correct GOARCH |
