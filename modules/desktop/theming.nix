{ config, pkgs, ... }:
{
  # Your existing packages
  environment.systemPackages = with pkgs; [
    nordic
    nordzy-cursor-theme
    nwg-look
    zafiro-icons
    papirus-icon-theme
    lxappearance
    libsForQt5.qt5ct
  ];

  # Configure default cursor theme system-wide
  environment.variables = {
    XCURSOR_THEME = "Nordzy-cursors";
    XCURSOR_SIZE = "24";
  };

  # Qt configuration
  qt.enable = true;
  qt.platformTheme = "qt5ct";
}
