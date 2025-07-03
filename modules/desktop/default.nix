{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./kitty.nix
    # Add any other desktop modules here
  ];
}
