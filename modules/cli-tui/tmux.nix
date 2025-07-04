{ config, pkgs, ... }:

{
  # System-level tmux installation
  environment.systemPackages = with pkgs; [
    tmux
    tmuxifier
  ];

  # Home-manager will handle the config via symlink
  programs.tmux.enable = true;
}
