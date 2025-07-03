{ config, lib, pkgs, inputs, ... }:

let
  # Get the directory where this flake is located
  dotfilesDir = "${config.home.homeDirectory}/nixos-config/dotfiles";
in
{
  imports = [
    ./git.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # Use the file attribute with string paths (this should work reliably)
  home.file = {
    ".config/doom".source = "${dotfilesDir}/doom";
    ".config/tmux".source = "${dotfilesDir}/tmux";
    ".config/hypr".source = "${dotfilesDir}/hypr";
    ".config/waybar".source = "${dotfilesDir}/waybar";
    ".config/swaync".source = "${dotfilesDir}/swaync";
    ".config/wofi".source = "${dotfilesDir}/wofi";
    ".config/nvim".source = "${dotfilesDir}/nvim";
    ".config/zathura".source = "${dotfilesDir}/zathura";
    ".tmux.conf".source = "${dotfilesDir}/tmux.conf";
  };
}
