{ config, pkgs, ... }:
{
  # Enable GDM display manager
  services.displayManager = {
    enable = true;
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
}
