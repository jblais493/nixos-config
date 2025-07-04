{ config, pkgs, ... }:
{
  # Enable GDM display manager
  services.xserver = {
    enable = true;
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
