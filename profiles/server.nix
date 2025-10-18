{ lib, ... }:
{
  imports = [
    ../modules/shared
    # NO desktop modules
    # NO audio modules
  ];

  time.timeZone = "UTC";

  networking.networkmanager.enable = lib.mkForce false;
  networking.useNetworkd = true;

  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  boot.loader.systemd-boot.configurationLimit = 5;

  # Server hardening
  security.sudo.wheelNeedsPassword = true;
  services.fail2ban.enable = true;
}
