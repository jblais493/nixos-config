{
  config,
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.impermanence.nixosModules.default
    ./disk-config.nix
    ../../profiles/server.nix
  ];

  networking.hostName = "empire";

  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
  };

  # Create subvolumes on first boot
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    mkdir -p /btrfs_tmp
    mount -t btrfs /dev/sda3 /btrfs_tmp

    if [ ! -d /btrfs_tmp/root ]; then
      btrfs subvolume create /btrfs_tmp/root
      btrfs subvolume create /btrfs_tmp/persist  
      btrfs subvolume create /btrfs_tmp/nix
    fi

    umount /btrfs_tmp
  '';

  # Manual filesystem mounts with subvol
  fileSystems = {
    "/" = {
      device = "/dev/sda3";
      fsType = "btrfs";
      options = [
        "subvol=root"
        "noatime"
      ];
    };

    "/persist" = {
      device = "/dev/sda3";
      fsType = "btrfs";
      options = [
        "subvol=persist"
        "compress=zstd"
        "noatime"
      ];
      neededForBoot = true;
    };

    "/nix" = {
      device = "/dev/sda3";
      fsType = "btrfs";
      options = [
        "subvol=nix"
        "compress=zstd"
        "noatime"
      ];
    };

    # Let disko handle these
    # "/boot" is auto-generated
    # "/persist/data" is auto-generated
  };

  # Rest of config...
  networking = {
    useDHCP = false;
    interfaces.eth0.useDHCP = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22
        80
        443
      ];
    };
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCWNto66rFbOvb1VDEDuZYdwHQPfKM7+EjpnHvs3eRr joshua@joshuablais.com"
  ];

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/log"
      "/var/lib/nixos"
      "/var/lib/systemd"
    ];
    files = [
      "/etc/machine-id"
    ];
  };

  time.timeZone = lib.mkForce "UTC";

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;
  };

  system.stateVersion = "25.05";
}
