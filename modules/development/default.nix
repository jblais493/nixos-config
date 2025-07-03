{ config, pkgs, ... }:
{

  # CLI/TUI tools
  environment.systemPackages = with pkgs; [
    # File management
    eza              # Better ls
    yazi             # Terminal file manager
    bat              # Better cat
    fd               # Better find
    ripgrep-all      # Better grep
    fzf              # Fuzzy finder
    neovim

    # Git tools
    lazygit          # Git TUI

    # System tools
    btop             # Better top
    trash-cli        # Safe rm
    tldr             # Simplified man pages
    fastfetch        # System info
  ];
}
