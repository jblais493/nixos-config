{ config, lib, pkgs, inputs, ... }:

let
  # Create a function to make out-of-store symlinks
  mkOutOfStoreSymlink = path: config.lib.file.mkOutOfStoreSymlink path;
in
{
  imports = [
    ./git.nix
    ./setup.nix
    ./browsers.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # Use mkOutOfStoreSymlink for live editing
  home.file = {
    ".config/doom".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/doom";
    ".zshrc".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/zsh/.zshrc";
    ".config/starship.toml".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/starship/starship.toml";
    ".config/tmux".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/tmux";
    ".config/hypr".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";
    ".config/kitty".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/kitty";
    ".config/waybar".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";
    ".config/swaync".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";
    ".config/wofi".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
    ".config/nvim".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/nvim";
    ".config/zathura".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/zathura";
    ".tmux.conf".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/tmux/.tmux.conf";
  };
}
