{ config, pkgs, ... }:
{
  imports = [
    ./applications.nix
    ./audio.nix
    ./bluetooth.nix
    ./boot.nix
    ./browsers.nix
    ./communication.nix
    ./display-manager.nix
    ./email.nix
    ./fonts.nix
    ./gaming.nix
    ./hyprland.nix
    ./kmonad.nix
    ./power.nix
    ./printing.nix
    ./storage.nix
    ./networking.nix
    ./theming.nix
  ];
}
