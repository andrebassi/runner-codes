package main

import (
	"flag"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"sync"
	"syscall"

	"github.com/mdlayher/vsock"
)

func main() {
	// Parse flags
	socketPath := flag.String("socket", "/tmp/fc-proxy.sock", "Unix socket path to listen on")
	guestCID := flag.Uint("cid", 3, "Guest CID for vsock")
	guestPort := flag.Uint("port", 5000, "Guest port for vsock")
	flag.Parse()

	log.Printf("LLM-Firecracker Vsock Proxy starting...")
	log.Printf("  Unix Socket: %s", *socketPath)
	log.Printf("  Guest CID:   %d", *guestCID)
	log.Printf("  Guest Port:  %d", *guestPort)

	// Setup signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		log.Println("Received shutdown signal, cleaning up...")
		os.Remove(*socketPath)
		os.Exit(0)
	}()

	// Remove existing socket
	os.Remove(*socketPath)

	// Create Unix socket listener
	listener, err := net.Listen("unix", *socketPath)
	if err != nil {
		log.Fatalf("Failed to listen on unix socket: %v", err)
	}
	defer listener.Close()
	defer os.Remove(*socketPath)

	log.Printf("Listening on unix socket: %s", *socketPath)
	log.Printf("Proxying to vsock CID=%d port=%d", *guestCID, *guestPort)

	// Accept connections
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Printf("Accept error: %v", err)
			continue
		}

		go handleProxy(conn, uint32(*guestCID), uint32(*guestPort))
	}
}

func handleProxy(localConn net.Conn, guestCID, guestPort uint32) {
	defer localConn.Close()

	log.Printf("New connection from %s", localConn.RemoteAddr())

	// Connect to guest via vsock
	guestConn, err := vsock.Dial(guestCID, guestPort, nil)
	if err != nil {
		log.Printf("Failed to connect to guest (CID=%d, port=%d): %v", guestCID, guestPort, err)
		return
	}
	defer guestConn.Close()

	log.Printf("Connected to guest (CID=%d, port=%d)", guestCID, guestPort)

	// Bidirectional copy
	var wg sync.WaitGroup
	wg.Add(2)

	// Local -> Guest
	go func() {
		defer wg.Done()
		n, err := io.Copy(guestConn, localConn)
		if err != nil {
			log.Printf("Error copying local->guest: %v", err)
		}
		log.Printf("Copied %d bytes from local to guest", n)
	}()

	// Guest -> Local
	go func() {
		defer wg.Done()
		n, err := io.Copy(localConn, guestConn)
		if err != nil {
			log.Printf("Error copying guest->local: %v", err)
		}
		log.Printf("Copied %d bytes from guest to local", n)
	}()

	wg.Wait()
	log.Printf("Connection closed")
}
