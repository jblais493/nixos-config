{ config, pkgs, ... }:
{
  imports = [
    ./music.nix
    ./video.nix
    ./pdf.nix
  ];

  # Common media packages
  environment.systemPackages = with pkgs; [
    # Image viewers
    feh
    imv

    # Audio tools
    alsa-utils

    # Document viewers
    zathura
    evince
  ];
}
