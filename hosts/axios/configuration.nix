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

  networking.hostName = "axios";

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.grub = {
    enable = true;
    devices = [ "/dev/sda" ];
  };

  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
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
