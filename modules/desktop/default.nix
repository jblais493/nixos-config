{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./kitty.nix
    ./fonts.nix
    # Add any other desktop modules here
  ];
}
