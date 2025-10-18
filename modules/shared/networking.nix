{
  config,
  pkgs,
  lib,
  ...
}:
{
  # Enable NetworkManager by default (workstations)
  # Servers can override with mkForce
  networking.networkmanager.enable = lib.mkDefault true;

  # Only install GUI tools if NetworkManager is enabled
  environment.systemPackages = lib.mkIf config.networking.networkmanager.enable (
    with pkgs;
    [
      networkmanager
      networkmanagerapplet
    ]
  );
}
