{ config, pkgs, ... }:
{
  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    bluez
    bluetui
    blueman
    bluez-tools
  ];
}
