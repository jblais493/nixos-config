{ config, pkgs, inputs, ... }:

{
  imports = [
    ./tmux.nix
    ./git.nix
    ./nvim.nix
    ./zsh.nix
    ./doom.nix
    ./hyprland.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "24.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;
}
