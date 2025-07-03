{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./kitty.nix
    ./wofi.nix
    # Add any other desktop modules here
  ];
}
