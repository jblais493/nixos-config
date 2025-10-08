{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpc
    ncmpcpp
    spotify
    spotdl
    # Audio tools
    audacity
    # Audio codecs
    ffmpeg-full
    flac
    lame
  ];

  # Ensure users can access audio devices
  users.groups.audio = {};
}
