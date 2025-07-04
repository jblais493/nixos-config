{ config, pkgs, ... }:
{
  imports = [
    ./hyprland.nix
    ./kmonad.nix
    ./kitty.nix
    ./fonts.nix
    ./audio.nix
    # Add any other desktop modules here
  ];
}
