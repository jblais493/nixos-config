{ lib, ... }:
{
  imports = [
    ../modules/shared
  ];

  # Servers always use UTC
  time.timeZone = "UTC";

  # Aggressive GC for servers
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  # Minimal boot generations
  boot.loader.systemd-boot.configurationLimit = 5;

  # Server hardening
  security.sudo.wheelNeedsPassword = true;

  # Disable NetworkManager on servers (use systemd-networkd)
  networking.networkmanager.enable = lib.mkForce false;
  networking.useNetworkd = true;

  # No GUI packages
  environment.systemPackages = lib.mkForce [ ];
}
