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
    devices = [ "/dev/sda" ];
    useOSProber = true;
  };

  # Setup keyfile
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };

  boot.loader.grub.enableCryptodisk = true;
  boot.initrd.luks.devices."cryptroot".keyFile = "/boot/crypto_keyfile.bin";
  networking.hostName = "king"; # Define your hostname.

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
