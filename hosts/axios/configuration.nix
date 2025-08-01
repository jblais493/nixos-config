{ config, pkgs, lib, inputs, ... }:
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

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  # Setup keyfile
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };

  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.luks.devices."luks-68e37805-7090-4a03-a958-7c3271384a93".keyFile = "/boot/crypto_keyfile.bin";
  networking.hostName = "axios";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  # services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
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
  description = "joshua";
  extraGroups = [ "networkmanager" "wheel" "uinput" "input" ];
  openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCWNto66rFbOvb1VDEDuZYdwHQPfKM7+EjpnHvs3eRr joshua@joshuablais.com"
  ];
};

  environment.systemPackages = with pkgs; [
    btrfs-progs
    btrbk
    compsize
  ];

  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  system.stateVersion = "25.05";
}
