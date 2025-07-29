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
    polkit_gnome
  ];

  # Global environment variables for theming
  # environment.variables = {
  #   # Cursor theme
  #   XCURSOR_THEME = "Nordzy-cursors-white";
  #   XCURSOR_SIZE = "24";
  #   # GTK themes
  #   GTK_THEME = "Nordic";
  #   # Qt theming
  #   QT_QPA_PLATFORMTHEME = "qt5ct";
  # };

  # Set cursor theme at login
  environment.loginShellInit = ''
    export XCURSOR_THEME=Nordzy-cursors-white
    export XCURSOR_SIZE=24
  '';

  # System-wide GTK configuration
  environment.etc = {
    "gtk-2.0/gtkrc" = {
      text = ''
        gtk-theme-name="Nordic"
        gtk-icon-theme-name="Zafiro-icons"
        gtk-cursor-theme-name="Nordzy-cursors-white"
        gtk-cursor-theme-size=24
        gtk-font-name="Sans 10"
      '';
    };

    "gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-theme-name=Nordic
        gtk-icon-theme-name=Zafiro-icons
        gtk-cursor-theme-name=Nordzy-cursors-white
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
        gtk-cursor-theme-name=Nordzy-cursors-white
        gtk-cursor-theme-size=24
        gtk-font-name=Sans 10
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  # Also set cursor in X11 resources
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
    Xcursor.theme: Nordzy-cursors-white
    Xcursor.size: 24
    EOF
  '';

  # Qt configuration
  qt.enable = true;
  qt.platformTheme = "qt5ct";
  qt.style = "gtk2";  # Makes Qt apps follow GTK theme

  # Enable dconf for GNOME applications
  programs.dconf.enable = true;

  # Enable polkit
  security.polkit.enable = true;

  # Use GNOME's polkit agent
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };
}
