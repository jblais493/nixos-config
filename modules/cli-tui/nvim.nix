{ config, pkgs, ... }:

{
  # System-level tmux installation
  environment.systemPackages = with pkgs; [
    nvim
  ];

  # Home-manager will handle the config via symlink
  programs.neovim.enable = true;
}
