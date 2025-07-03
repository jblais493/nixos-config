{ config, pkgs, ... }:

{
  # Install tmux and related tools
  environment.systemPackages = with pkgs; [
    tmux
  ];

  # Create your exact tmux configuration
  environment.etc."tmux.conf".text = ''
    # Prefix configuration - C-Space instead of C-b
    unbind C-b
    set -g prefix C-Space
    bind C-Space send-prefix

    # Reload config file
    bind r source-file ~/.tmux.conf \; display-message "tmux.conf reloaded"

    # Status bar at top
    set-option -g status-position top

    # Movement with Ctrl+arrows
    bind-key -n C-Left select-pane -L
    bind-key -n C-Down select-pane -D
    bind-key -n C-Right select-pane -R
    bind-key -n C-Up select-pane -U

    # Vi-style key bindings
    set-window-option -g mode-keys vi
    bind-key v copy-mode
    bind-key -T copy-mode-vi v send-keys -X begin-selection
    bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
    bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel

    # Pane resizing with Shift+arrows
    bind -n S-Left resize-pane -L 2
    bind -n S-Right resize-pane -R 2
    bind -n S-Down resize-pane -D 1
    bind -n S-Up resize-pane -U 1

    # Smart splitting - preserves current path
    bind-key - split-window -v -c '#{pane_current_path}'
    bind-key \\ split-window -h -c '#{pane_current_path}'

    # New window in current path
    bind c new-window -c "#{pane_current_path}"

    # Break pane
    bind-key b break-pane -d

    # Quick note taking
    bind-key t split-window -h -c 'nvim ~/Vaults/inbox.md'

    # Start windows and panes at 1, not 0
    set -g base-index 1
    setw -g pane-base-index 1

    # Terminal and color settings
    set -g default-terminal "tmux-256color"
    set -ag terminal-overrides ",xterm-256color:RGB"

    # Mouse support
    set -g mouse on
  '';

  # Copy your tmux plugin directory structure
  system.activationScripts.tmux-plugins = ''
    mkdir -p /home/joshua/.config/tmux/plugins/catppuccin
    # You can add your catppuccin theme files here if needed
  '';
}
