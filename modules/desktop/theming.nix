{ config, pkgs, ... }:
{
  # Nordic theming packages
  environment.systemPackages = with pkgs; [
    nordic
    nordzy-cursor-theme
    nwg-look
    zafiro-icons
    papirus-icon-theme
    lxappearance
    libsForQt5.qt5ct
  ];

  # Global environment variables for theming
  environment.variables = {
    # Cursor theme
    XCURSOR_THEME = "Nordzy-cursors";
    XCURSOR_SIZE = "24";

    # GTK themes
    GTK_THEME = "Nordic";

    # Qt theming
    QT_QPA_PLATFORMTHEME = "qt5ct";
  };

  # System-wide GTK configuration
  environment.etc = {
    "gtk-2.0/gtkrc" = {
      text = ''
        gtk-theme-name="Nordic"
        gtk-icon-theme-name="Zafiro-icons"
        gtk-cursor-theme-name="Nordzy-cursors"
        gtk-cursor-theme-size=24
        gtk-font-name="Sans 10"
      '';
    };

    "gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-theme-name=Nordic
        gtk-icon-theme-name=Zafiro-icons
        gtk-cursor-theme-name=Nordzy-cursors
        gtk-cursor-theme-size=24
        gtk-font-name=Sans 10
        gtk-application-prefer-dark-theme=1
      '';
    };

    "gtk-4.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-theme-name=Nordic
        gtk-icon-theme-name=Zafiro-icons
        gtk-cursor-theme-name=Nordzy-cursors
        gtk-cursor-theme-size=24
        gtk-font-name=Sans 10
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  # Qt configuration
  qt.enable = true;
  qt.platformTheme = "qt5ct";
  qt.style = "gtk2";  # Makes Qt apps follow GTK theme

  # Enable dconf for GNOME applications
  programs.dconf.enable = true;
}
