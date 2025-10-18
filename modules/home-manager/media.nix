{ config, ... }:
{
  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/MusicOrganized";
  };
}
