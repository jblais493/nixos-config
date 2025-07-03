# This is a placeholder hardware configuration
# You'll need to generate the real one when you install NixOS
{ config, lib, pkgs, modulesPath, ... }:
{
  imports = [ ];

  # Placeholder hardware config
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Placeholder filesystem config
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/placeholder";
    fsType = "ext4";
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
