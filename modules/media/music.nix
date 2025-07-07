{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    mpc
    ncmpcpp
    spotifyd
    spotdl

    # Audio tools
    audacity

    # Audio codecs
    ffmpeg-full
    flac
    lame
  ];
}
