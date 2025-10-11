{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/cli-tui
    ../../modules/shared
    ../../modules/security
    ../../modules/services
  ];

  # Host-specific configuration
  networking.hostName = "empirica";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Define your user properly
  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Localization
  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

    # Enable the OpenSSH daemon.
  services.openssh.enable = true;

    # Disable all systemd sleep/suspend/hibernate targets
  systemd.targets = {
    sleep.enable = false;
    suspend.enable = false;
    hibernate.enable = false;
    hybrid-sleep.enable = false;
  };

  # Configure systemd-logind to ignore power events
services.logind.extraConfig = ''
  HandlePowerKey=ignore
  IdleAction=ignore
'';

  # Prevent automatic suspension
  powerManagement = {
    enable = false;  # Disable NixOS power management entirely
  };

  system.stateVersion = "25.05";
}
