{ config, pkgs, ... }:
{
  # Enable GDM display manager
  services = {
    enable = true;
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
}
