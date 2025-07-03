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

  # Use relative paths from the flake root
  home.file = {
    ".config/doom".source = ../../dotfiles/doom;
    ".config/tmux".source = ../../dotfiles/tmux;
    ".config/hypr".source = ../../dotfiles/hypr;
    ".config/waybar".source = ../../dotfiles/waybar;
    ".config/swaync".source = ../../dotfiles/swaync;
    ".config/wofi".source = ../../dotfiles/wofi;
    ".config/nvim".source = ../../dotfiles/nvim;
    ".config/zathura".source = ../../dotfiles/zathura;
    ".tmux.conf".source = ../../dotfiles/tmux/.tmux.conf;
  };
}
