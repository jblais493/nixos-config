{ config, pkgs, inputs, ... }:
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
  enable = true;
  device = "/dev/sda"; # Install GRUB to MBR
  enableCryptodisk = true; # Enable LUKS support in GRUB
  };

  boot.initrd.luks.devices = {
    crypted = {
    device = "/dev/disk/by-partlabel/luks";
    allowDiscards = true;
    preLVM = true;
    };
  };

  # Btrfs maintenance
  services.btrfs.autoScrub = {
    enable = true;
    interval = "monthly";
    fileSystems = [ "/" ];
  };

  # Enable periodic snapshots with btrbk
  services.btrbk.instances.btrbk = {
    onCalendar = "daily";
    settings = {
      timestamp_format = "long";
      preserve_day_of_week = "monday";

      volume."/" = {
        target = "/snapshots";
        subvolume = {
          "home" = {
            snapshot_preserve_min = "2d";
            snapshot_preserve = "14d 8w 12m";
          };
          "persist" = {
            snapshot_preserve_min = "2d";
            snapshot_preserve = "14d 8w 12m";
          };
        };
      };
    };
  };

  # Enable SSH immediately
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  networking.firewall.allowedTCPPorts = [ 22 ];

  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
    extraGroups = [ "networkmanager" "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCWNto66rFbOvb1VDEDuZYdwHQPfKM7+EjpnHvs3eRr joshua@joshuablais.com"
    ];
  };

  users.groups.joshua = {};

  # Enable compression and btrfs tools
  environment.systemPackages = with pkgs; [
    btrfs-progs
    btrbk
    compsize
  ];

  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  system.stateVersion = "25.05";
}
