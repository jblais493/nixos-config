{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # File managers
    xfce.thunar
    xfce.tumbler

    # Communication
    signal-desktop
    telegram-desktop
    thunderbird

    # Graphics
    gimp-with-plugins

    # Productivity
    libreoffice

    # System utilities
    brightnessctl
    libnotify
    xdg-utils

    # VPN
    mullvad-vpn
    mullvad

    # Others
    qbittorrent
    syncthing
    flatpak
  ];

  # Enable flatpak
  services.flatpak.enable = true;
}
