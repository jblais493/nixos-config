{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    ncmpcpp
    spotifyd

    # Audio tools
    audacity
    lmms

    # Audio codecs
    ffmpeg
    flac
    lame
  ];
}
