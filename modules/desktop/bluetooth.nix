{ config, pkgs, ... }:
{
  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    bluez
    blueman
    bluez-tools
  ];
}
