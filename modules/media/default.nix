{ config, pkgs, ... }:
{
  imports = [
    ./images.nix
    ./music.nix
    ./pdf.nix
    ./video.nix
  ];
}
