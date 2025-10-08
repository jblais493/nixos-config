{ config, pkgs, ... }:

let
  unstable = import <nixpkgs-unstable> {
    config = config.nixpkgs.config;
  };
in
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpc
    ncmpcpp
    spotify
    unstable.spotdl  # Pull from unstable
    # Audio tools
    audacity
    # Audio codecs
    ffmpeg-full
    flac
    lame
  ];

  users.groups.audio = {};
}
