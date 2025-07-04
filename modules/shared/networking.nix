{ config, pkgs, ... }:
{
  # Enable NetworkManager
  networking.networkmanager.enable = true;

  # Packages
  environment.systemPackages = with pkgs; [
    networkmanager
    networkmanagerapplet
  ];
}
