{ config, pkgs, lib, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./disk-configuration.nix
    ../../modules/desktop
    ../../modules/shared
    ../../modules/cli-tui
    ../../modules/development
    ../../modules/media
  ];

  networking.hostName = "axios";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Boot configuration for encrypted setup
  boot.loader.grub = {
    enable = lib.mkForce true;
    devices = lib.mkForce [ "/dev/sda" ];
    enableCryptodisk = lib.mkForce true;
  };

  # Rest of your configuration...
  services.btrfs.autoScrub = {
    enable = true;
    interval = "monthly";
    fileSystems = [ "/" ];
  };

# SSH configuration
services.openssh = {
  enable = true;
  settings = {
    PermitRootLogin = "no";  # Change this to "no" for security
    PasswordAuthentication = true;  # Enable password auth temporarily
  };
};

  networking.firewall.allowedTCPPorts = [ 22 ];

users.users.joshua = {
  isNormalUser = true;
  description = "Joshua Blais";
  group = "joshua";
  extraGroups = [ "networkmanager" "wheel" ];
  password = "nixos";  # TODO setup ageinx
  openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCWNto66rFbOvb1VDEDuZYdwHQPfKM7+EjpnHvs3eRr joshua@joshuablais.com"
  ];
};

  users.groups.joshua = {};

  environment.systemPackages = with pkgs; [
    btrfs-progs
    btrbk
    compsize
  ];

  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  system.stateVersion = "25.05";
}
