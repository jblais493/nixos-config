{ config, pkgs, ... }:
{
  # Proper cursor configuration - the NixOS way
  home.pointerCursor = {
    name = "Nordzy-cursors-white";  # This is the correct name
    package = pkgs.nordzy-cursor-theme;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  # GTK theming the declarative way
  gtk = {
    enable = true;
    theme = {
      name = "Nordic";
      package = pkgs.nordic;
    };
    iconTheme = {
      name = "Zafiro-icons";
      package = pkgs.zafiro-icons;
    };
    cursorTheme = {
      name = "Nordzy-cursors-white";
      package = pkgs.nordzy-cursor-theme;
      size = 24;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };

  # Qt follows GTK - the principle of least surprise
  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      name = "gtk2";
    };
  };

  # Hyprland-specific cursor fixes (since you're using Hyprland)
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "gsettings set org.gnome.desktop.interface cursor-theme 'Nordzy-cursors-white'"
      "gsettings set org.gnome.desktop.interface cursor-size 24"
    ];
  };
}
