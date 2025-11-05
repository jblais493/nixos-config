// ~/.config/hypr/scripts/emacs-launcher.go
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

type Client struct {
	Class     string `json:"class"`
	Workspace struct {
		ID int `json:"id"`
	} `json:"workspace"`
}

type ActiveWorkspace struct {
	ID int `json:"id"`
}

func getEmacsWorkspace() (int, bool) {
	cmd := exec.Command("hyprctl", "clients", "-j")
	output, err := cmd.Output()
	if err != nil {
		return 0, false
	}

	var clients []Client
	if err := json.Unmarshal(output, &clients); err != nil {
		return 0, false
	}

	for _, client := range clients {
		if strings.Contains(client.Class, "Emacs") {
			return client.Workspace.ID, true
		}
	}
	return 0, false
}

func getCurrentWorkspace() (int, error) {
	cmd := exec.Command("hyprctl", "activeworkspace", "-j")
	output, err := cmd.Output()
	if err != nil {
		return 0, err
	}

	var workspace ActiveWorkspace
	if err := json.Unmarshal(output, &workspace); err != nil {
		return 0, err
	}
	return workspace.ID, nil
}

func executeEmacsCommand(command string) error {
	cmd := exec.Command("emacsclient", "-n", "-e", command)
	return cmd.Run()
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: emacs-launcher <elisp-command>")
		fmt.Fprintln(os.Stderr, "Example: emacs-launcher '(universal-launcher-popup)'")
		os.Exit(1)
	}

	// Join all arguments in case command has spaces
	emacsCommand := strings.Join(os.Args[1:], " ")

	// Find Emacs workspace
	targetWS, found := getEmacsWorkspace()
	if !found {
		// No Emacs window found - just execute command
		// This will either open a new frame or fail gracefully
		if err := executeEmacsCommand(emacsCommand); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: emacsclient failed: %v\n", err)
			os.Exit(1)
		}
		return
	}

	// Check current workspace
	currentWS, err := getCurrentWorkspace()
	if err != nil {
		// Can't determine current workspace, execute anyway
		executeEmacsCommand(emacsCommand)
		return
	}

	// If already on Emacs workspace, execute immediately
	if currentWS == targetWS {
		executeEmacsCommand(emacsCommand)
		return
	}

	// Switch to Emacs workspace
	switchCmd := exec.Command("hyprctl", "dispatch", "workspace", fmt.Sprintf("%d", targetWS))
	if err := switchCmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: workspace switch failed: %v\n", err)
		// Execute anyway
		executeEmacsCommand(emacsCommand)
		return
	}

	// Wait for workspace switch to complete with intelligent polling
	// Start with fast polls, back off if taking longer
	maxWait := 500 * time.Millisecond
	pollInterval := 2 * time.Millisecond // Start very fast
	deadline := time.Now().Add(maxWait)

	for time.Now().Before(deadline) {
		current, err := getCurrentWorkspace()
		if err == nil && current == targetWS {
			// Workspace switch confirmed - execute command
			executeEmacsCommand(emacsCommand)
			return
		}

		time.Sleep(pollInterval)

		// Exponential backoff: 2ms -> 4ms -> 8ms -> 10ms (cap)
		pollInterval *= 2
		if pollInterval > 10*time.Millisecond {
			pollInterval = 10 * time.Millisecond
		}
	}

	// Timeout reached - execute anyway (workspace switch probably worked)
	executeEmacsCommand(emacsCommand)
}
