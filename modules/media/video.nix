{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Video players
    mpv
    vlc

    # Video editing
    obs-studio
    shotcut

    # Anime CLI:
    ani-cli

    # Video tools
    ffmpeg
    yt-dlp
  ];
}
