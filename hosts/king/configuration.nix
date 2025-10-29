{
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    # ./hardware-configuration.nix
    inputs.disko.nixosModules.disko
    ./disko.nix
    # ../../modules/desktop
    ../../modules/shared
    ../../modules/cli-tui
    # ../../modules/development
    # ../../modules/media
  ];

  networking.hostName = "king"; # Define your hostname.

  boot.initrd.luks.devices."cryptroot" = {
    device = "/dev/disk/by-partlabel/disk-main-luks";
    allowDiscards = true;
    bypassWorkqueues = true;
  };

  boot.initrd.availableKernelModules = [
    "virtio_pci"
    "virtio_blk"
    "virtio_scsi"
  ];

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  boot.loader.grub = {
    enable = true;
    useOSProber = true;
    enableCryptodisk = true;
  };

  users.users.joshua = {
    isNormalUser = true;
    description = "Joshua Blais";
    group = "joshua";
    initialPassword = "changeme";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  users.groups.joshua = { };

  time.timeZone = "America/Edmonton";

  system.stateVersion = "25.05";
}
