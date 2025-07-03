{ config, pkgs, inputs, ... }:

{
  imports = [
    ./git.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "24.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/doom";

  home.file.".config/+STORE".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/+STORE";

  home.file.".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/tmux.conf";

  home.file.".config/tmux".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/tmux";

  home.file.".config/hypr".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";

  home.file.".config/waybar".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";

  home.file.".config/swaync".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";

  home.file.".config/wofi".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
}
