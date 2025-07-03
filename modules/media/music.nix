{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    ncmpcpp
    spotifyd

    # Audio tools
    audacity

    # Audio codecs
    ffmpeg
    flac
    lame
  ];
}
