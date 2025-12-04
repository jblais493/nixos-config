{ config, pkgs, ... }:
{
  imports = [
    ./applications.nix
    ./audio.nix
    # ./backup.nix
    ./blocklist.nix
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
    ./networking.nix
    ./power.nix
    ./printing.nix
    ./storage.nix
    ./theming.nix
  ];
}
