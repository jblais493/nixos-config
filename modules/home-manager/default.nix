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

  # Symlink dotfiles (removing +STORE for now)
  home.file = {
    ".config/doom".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/doom";

    ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/tmux.conf";

    ".config/tmux".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/tmux";

    ".config/hypr".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";

    ".config/waybar".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";

    ".config/swaync".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";

    ".config/wofi".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
  };
}
