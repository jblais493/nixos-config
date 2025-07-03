{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    ncmpcpp
    spotify

    # Audio tools
    audacity
    lmms

    # Audio codecs
    ffmpeg
    flac
    lame
  ];
}
