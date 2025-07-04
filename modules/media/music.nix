{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    ncmpcpp
    spotifyd
    spotdl

    # Audio tools
    audacity

    # Audio codecs
    ffmpeg
    flac
    lame
  ];
}
