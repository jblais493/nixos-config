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

  # Use XDG config files instead of home.file for better compatibility
  xdg.configFile = {
    "doom".source = "${config.home.homeDirectory}/nixos-config/dotfiles/doom";
    "tmux".source = "${config.home.homeDirectory}/nixos-config/dotfiles/tmux";
    "hypr".source = "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";
    "waybar".source = "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";
    "swaync".source = "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";
    "wofi".source = "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
    "nvim".source = "${config.home.homeDirectory}/nixos-config/dotfiles/nvim";
    "zathura".source = "${config.home.homeDirectory}/nixos-config/dotfiles/zathura";
  };

  # Handle the root-level files separately
  home.file = {
    ".tmux.conf".source = "${config.home.homeDirectory}/nixos-config/dotfiles/tmux.conf";
  };
}
