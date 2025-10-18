{ lib, ... }:
{
  imports = [
    ../modules/shared
    ../modules/desktop/hyprland.nix
    ../modules/desktop/audio.nix
    ../modules/desktop/gaming.nix
    ../modules/cli-tui/security.nix
    ../modules/cli-tui/development.nix
  ];

  time.timeZone = lib.mkDefault "America/Edmonton";
  nix.gc.options = "--delete-older-than 30d";
  boot.loader.systemd-boot.configurationLimit = 20;
}
