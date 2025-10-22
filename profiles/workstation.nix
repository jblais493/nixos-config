{ lib, ... }:
{
  imports = [
    ../modules/shared
    ../modules/media
    ../modules/desktop
    ../../modules/cli-tui
    ../modules/desktop/gaming.nix
    ../modules/cli-tui/security.nix
    ../modules/cli-tui/development.nix
  ];

  time.timeZone = lib.mkDefault "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";
  nix.gc.options = "--delete-older-than 30d";
  boot.loader.systemd-boot.configurationLimit = 20;
}
