{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    kmonad
  ];

  # Setup Kmonad
  boot.kernelModules = [ "uinput" ];

  services.udev.extraRules = ''
    # KMonad user access to /dev/uinput
    KERNEL=="uinput", MODE="0660", GROUP="input", TAG+="uaccess"
  '';

  # Add your user to the input group
  users.users.joshua.extraGroups = [ "input" ];
}
