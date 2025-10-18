{ lib, ... }:
{
  imports = [
    ../modules/shared
  ];

  # Workstation uses local timezone
  time.timeZone = lib.mkDefault "America/Edmonton";

  # Conservative GC for development work
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Keep more boot generations for safety
  boot.loader.systemd-boot.configurationLimit = lib.mkDefault 20;

  # Workstation-specific: allow wheel group for sudo
  security.sudo.wheelNeedsPassword = lib.mkDefault true;
}
