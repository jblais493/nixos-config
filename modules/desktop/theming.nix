{ config, pkgs, ... }:
{
  # Nord theming packages
  environment.systemPackages = with pkgs; [
    # Nordic GTK theme
    nordic

    # Nord cursor theme
    nordzy-cursor-theme

    # Icon themes
    zafiro-icons    # Zafiro icon theme
    papirus-icon-theme  # Alternative/fallback icons

    # Theme tools
    lxappearance    # GTK theme selector
    libsForQt5.qt5ct          # Qt theme configuration
    kvantum        # Qt theme engine
  ];

  # Enable Qt theming
  qt.enable = true;
  qt.platformTheme = "qt5ct";
  qt.style = "kvantum";
}
