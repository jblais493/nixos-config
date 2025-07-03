{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Video players
    mpv
    vlc

    # Video editing
    kdenlive
    obs-studio

    # Video tools
    ffmpeg
    youtube-dl
    yt-dlp
  ];
}
