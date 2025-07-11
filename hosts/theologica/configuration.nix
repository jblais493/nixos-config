{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/desktop
    ../../modules/shared
    ../../modules/cli-tui
    ../../modules/development
    ../../modules/media
    ../../modules/security
  ];

  # Host-specific configuration
  networking.hostName = "theologica";

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Boot loader configuration
  boot.loader.grub = {
    enable = true;
    devices = [ "/dev/sda" ]; # Change this to your actual boot device
    # Or use systemd-boot instead:
    # boot.loader.systemd-boot.enable = true;
    # boot.loader.efi.canTouchEfiVariables = true;
  };

  # Define your user properly
  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Create the user group
  users.groups.joshua = {};

  # Basic system configuration
  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  # Set the state version
  system.stateVersion = "25.05";
}
