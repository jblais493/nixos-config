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
    kdePackages.kdeconnect-kde

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

  programs.kdeconnect.enable = true;

  # Firewall rules for kdeconnect
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  # Enable flatpak
  services.flatpak.enable = true;
}
