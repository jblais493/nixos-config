{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./fonts.nix
    ./audio.nix
    ./bluetooth.nix
    ./printing.nix
    ./browsers.nix
    ./applications.nix
    ./email.nix
    ./gaming.nix
    ./display-manager.nix
    ./storage.nix
    ./theming.nix
    ./boot.nix
    ./power.nix
    ./../secrets.nix
  ];
}
