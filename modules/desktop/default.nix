{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./kitty.nix
    ./fonts.nix
    ./audio.nix
    ./bluetooth.nix
    ./printing.nix
    ./browsers.nix
    ./applications.nix
    ./display-manager.nix
    ./storage.nix
  ];
}
