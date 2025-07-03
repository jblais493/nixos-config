{ config, lib, pkgs, inputs, ... }:
{
  imports = [
    ./git.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # Use XDG config files with hardcoded paths
  xdg.configFile = {
    "doom".source = "/home/joshua/nixos-config/dotfiles/doom";
    "tmux".source = "/home/joshua/nixos-config/dotfiles/tmux";
    "hypr".source = "/home/joshua/nixos-config/dotfiles/hypr";
    "waybar".source = "/home/joshua/nixos-config/dotfiles/waybar";
    "swaync".source = "/home/joshua/nixos-config/dotfiles/swaync";
    "wofi".source = "/home/joshua/nixos-config/dotfiles/wofi";
    "nvim".source = "/home/joshua/nixos-config/dotfiles/nvim";
    "zathura".source = "/home/joshua/nixos-config/dotfiles/zathura";
  };

  # Handle the root-level files separately
  home.file = {
    ".tmux.conf".source = "/home/joshua/nixos-config/dotfiles/tmux.conf";
  };
}
