{ config, pkgs, ... }:

{
  imports = [
    ./zsh.nix
    ./tmux.nix
  ];

  # Tools that don't need configuration - just install them
  environment.systemPackages = with pkgs; [
    # Basic CLIs
    eza              # Better ls
    zoxide           # Better cd
    yazi             # Terminal file manager
    bat              # Better cat
    fd               # Better find
    ripgrep-all      # Better grep
    fzf              # Fuzzy finder
    wget
    curl
    killall
    zip
    unzip
    jq
    rsync
    borgbackup
    coreutils
    ydotool
    gnupg
    age

    # Git tools
    lazygit          # Git TUI

    # Pass
    pass

    # System tools
    btop             # Better top
    trash-cli        # Safe rm
    tldr             # Simplified man pages
    fastfetch        # System info

    # Development containers
    podman           # Container runtime
    podman-compose   # Docker-compose for podman
  ];

  # Enable podman
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;  # Create docker alias
    defaultNetwork.settings.dns_enabled = true;
  };
}
