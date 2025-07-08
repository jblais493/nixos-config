{ config, lib, pkgs, ... }:

{
  # Fast, beautiful Plymouth boot with NixOS branding
  boot.plymouth = {
    enable = true;
    theme = "breeze";
  };

  # Silent boot for speed and clean look
  boot.kernelParams = [
    "quiet"
    "splash"
    "loglevel=3"
    "rd.systemd.show_status=false"
    "rd.udev.log_level=3"
    "systemd.show_status=auto"
  ];

  boot.consoleLogLevel = 0;

  # Fast boot timeout (updated option name)
  boot.loader.timeout = 1;

  # Beautiful GRUB theme
  boot.loader.grub.theme = pkgs.nixos-grub2-theme;

  # Keep boot entries clean
  boot.loader.grub.configurationLimit = 5;

  # LUKS + Plymouth integration for themed password prompts
  boot.initrd.systemd.enable = true;
}
