{ config, pkgs, ... }:
{
  # Enable NetworkManager
  networking.networkmanager.enable = true;

  # Packages
  environment.systemPackages = with pkgs; [
    networkmanager
    networkmanagerapplet
  ];

  networking.extraHosts = ''
    100.76.139.86    bin.labrynth.org
    100.76.139.86    cal.labrynth.org
  '';
}
