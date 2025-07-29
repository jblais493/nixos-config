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
    element-desktop

    # Graphics
    gimp-with-plugins

    # KDE connect
    libsForQt5.kdeconnect-kde

    # Productivity
    libreoffice

    # System utilities
    brightnessctl
    libnotify
    xdg-utils
    gammastep

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
