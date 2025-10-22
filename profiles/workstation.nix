{ lib, ... }:
{
  imports = [
    ../modules/cli-tui
    ../modules/development
    ../modules/desktop
    ../modules/media
    ../modules/security
    ../modules/shared
  ];

  time.timeZone = lib.mkDefault "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";
  nix.gc.options = "--delete-older-than 30d";
  boot.loader.systemd-boot.configurationLimit = 20;
}
