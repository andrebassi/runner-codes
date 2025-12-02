# LLM-FireSandbox Makefile
# Build automation for host-agent, guest-runner, vsock-proxy and infra.operator

.PHONY: all build clean test help
.PHONY: build-host-agent build-guest-runner build-proxy build-cli
.PHONY: build-guest-runner-amd64 build-guest-runner-arm64

# Directories
BIN_DIR := bin
ROOTFS_DIR := rootfs

# Go build flags
GO := go
GOFLAGS := -ldflags="-s -w"

# Architecture detection
ARCH := $(shell uname -m)
ifeq ($(ARCH),x86_64)
    GUEST_GOARCH := amd64
else ifeq ($(ARCH),aarch64)
    GUEST_GOARCH := arm64
else ifeq ($(ARCH),arm64)
    GUEST_GOARCH := arm64
else
    GUEST_GOARCH := amd64
endif

# Default target
all: build

# Help
help:
	@echo "LLM-FireSandbox Build System"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all                    Build all components"
	@echo "  build                  Build all components"
	@echo "  build-host-agent       Build host-agent for current OS"
	@echo "  build-guest-runner     Build guest-runner for detected guest arch ($(GUEST_GOARCH))"
	@echo "  build-guest-runner-amd64  Build guest-runner for x86_64"
	@echo "  build-guest-runner-arm64  Build guest-runner for ARM64"
	@echo "  build-proxy            Build vsock-proxy"
	@echo "  build-cli              Build infra.operator CLI"
	@echo "  build-cli-linux        Build infra.operator CLI for Linux"
	@echo "  clean                  Remove built binaries"
	@echo "  test                   Run tests"
	@echo "  deps                   Download Go dependencies"
	@echo ""
	@echo "infra.operator CLI commands:"
	@echo "  infra.operator rootfs create --lang python"
	@echo "  infra.operator snapshot create --lang python"
	@echo "  infra.operator run --lang python --code 'print(1)'"
	@echo "  infra.operator api --port 8080"
	@echo "  infra.operator benchmark --all"
	@echo ""
	@echo "Detected guest architecture: $(GUEST_GOARCH)"

# Create directories
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(ROOTFS_DIR):
	mkdir -p $(ROOTFS_DIR)

# Build all
build: $(BIN_DIR) $(ROOTFS_DIR) build-host-agent build-guest-runner build-proxy build-cli
	@echo "Build complete!"
	@echo ""
	@echo "Binaries:"
	@ls -la $(BIN_DIR)/
	@echo ""
	@echo "Guest runner (for rootfs):"
	@ls -la $(ROOTFS_DIR)/guest-runner 2>/dev/null || echo "  Not built yet"

# Build infra.operator CLI
build-cli: $(BIN_DIR)
	@echo "Building infra.operator CLI..."
	$(GO) build $(GOFLAGS) -o $(BIN_DIR)/infra.operator ./cmd/infra.operator
	@echo "Built: $(BIN_DIR)/infra.operator"

# Build infra.operator for Linux (for deployment)
build-cli-linux: $(BIN_DIR)
	@echo "Building infra.operator CLI for Linux/amd64..."
	GOOS=linux GOARCH=amd64 $(GO) build $(GOFLAGS) -o $(BIN_DIR)/infra.operator-linux-amd64 ./cmd/infra.operator
	@echo "Built: $(BIN_DIR)/infra.operator-linux-amd64"

# Build host-agent (runs on host)
build-host-agent: $(BIN_DIR)
	@echo "Building host-agent..."
	cd host-agent && $(GO) build $(GOFLAGS) -o ../$(BIN_DIR)/host-agent .
	@echo "Built: $(BIN_DIR)/host-agent"

# Build guest-runner for detected architecture
build-guest-runner: $(ROOTFS_DIR)
	@echo "Building guest-runner for Linux/$(GUEST_GOARCH)..."
	cd guest-runner && GOOS=linux GOARCH=$(GUEST_GOARCH) $(GO) build $(GOFLAGS) -o ../$(ROOTFS_DIR)/guest-runner .
	@echo "Built: $(ROOTFS_DIR)/guest-runner ($(GUEST_GOARCH))"

# Build guest-runner for x86_64
build-guest-runner-amd64: $(ROOTFS_DIR)
	@echo "Building guest-runner for Linux/amd64..."
	cd guest-runner && GOOS=linux GOARCH=amd64 $(GO) build $(GOFLAGS) -o ../$(ROOTFS_DIR)/guest-runner-amd64 .
	@echo "Built: $(ROOTFS_DIR)/guest-runner-amd64"

# Build guest-runner for ARM64
build-guest-runner-arm64: $(ROOTFS_DIR)
	@echo "Building guest-runner for Linux/arm64..."
	cd guest-runner && GOOS=linux GOARCH=arm64 $(GO) build $(GOFLAGS) -o ../$(ROOTFS_DIR)/guest-runner-arm64 .
	@echo "Built: $(ROOTFS_DIR)/guest-runner-arm64"

# Build vsock-proxy
build-proxy: $(BIN_DIR)
	@echo "Building vsock-proxy..."
	cd proxy && $(GO) build $(GOFLAGS) -o ../$(BIN_DIR)/vsock-proxy .
	@echo "Built: $(BIN_DIR)/vsock-proxy"

# Download dependencies
deps:
	@echo "Downloading dependencies..."
	cd host-agent && $(GO) mod download
	cd guest-runner && $(GO) mod download
	cd proxy && $(GO) mod download
	@echo "Dependencies downloaded"

# Run tests
test:
	@echo "Running tests..."
	cd host-agent && $(GO) test -v ./...
	cd guest-runner && $(GO) test -v ./...
	cd proxy && $(GO) test -v ./...

# Clean
clean:
	@echo "Cleaning..."
	rm -rf $(BIN_DIR)
	rm -f $(ROOTFS_DIR)/guest-runner
	rm -f $(ROOTFS_DIR)/guest-runner-amd64
	rm -f $(ROOTFS_DIR)/guest-runner-arm64
	@echo "Cleaned"

# Tidy go modules
tidy:
	cd host-agent && $(GO) mod tidy
	cd guest-runner && $(GO) mod tidy
	cd proxy && $(GO) mod tidy
