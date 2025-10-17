{ config, pkgs, inputs, ... }:
{
  imports = [
    # ./hardware-configuration.nix
    inputs.disko.nixosModules.disko
    ./disko.nix
    ../../modules/desktop
    ../../modules/shared
    ../../modules/cli-tui
    ../../modules/development
    ../../modules/media
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
    };
  };

  boot.loader.grub = {
    enable = true;
    useOSProber = true;
    enableCryptodisk = true;  # Moved this up for clarity
  };

  networking.hostName = "king"; # Define your hostname.

  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
    initialPassword = "changeme";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  users.groups.joshua = {};

  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  environment.systemPackages = with pkgs; [
    vim
    git
  ];

  system.stateVersion = "25.05";
}
