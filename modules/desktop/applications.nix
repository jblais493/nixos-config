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
    gimp3-with-plugins

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

  services.mullvad-vpn.enable = true;

  programs.kdeconnect.enable = true;

systemd.user.services.kdeconnect = {
    description = "KDE Connect daemon";
    wantedBy = [ "default.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.kdePackages.kdeconnect-kde}/bin/kdeconnectd";
      Restart = "on-failure";
      RestartSec = 3;
    };
    environment = {
      # Ensure proper DBus session
      DISPLAY = ":0";
    };
  };

  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  # Enable flatpak
  services.flatpak.enable = true;
}
