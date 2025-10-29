{ config, pkgs, ... }:
{
  # Proper cursor configuration - the NixOS way
  home.pointerCursor = {
    name = "Nordzy-cursors-white"; # This is the correct name
    package = pkgs.nordzy-cursor-theme;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  gtk = {
    enable = true;

    iconTheme = {
      name = "Zafiro-icons-Dark";
      package = pkgs.zafiro-icons;
    };
  };

  # gtk = {
  #   enable = true;
  #   theme = {
  #     name = "Nordic-darker";
  #     package = pkgs.nordic;
  #   };
  #   iconTheme = {
  #     name = "Zafiro-icons-Dark";
  #     package = pkgs.zafiro-icons;
  #   };
  #   cursorTheme = {
  #     name = "Nordzy-cursors";
  #     package = pkgs.nordzy-cursor-theme;
  #     size = 24;
  #   };
  #  gtk3.extraConfig = {
  #    gtk-application-prefer-dark-theme = 1;
  #  };
  #  gtk4.extraConfig = {
  #    gtk-application-prefer-dark-theme = 1;
  #  };
  # };

  # Qt follows GTK - the principle of least surprise
  # qt = {
  #   enable = true;
  #   platformTheme.name = "gtk";
  #   style = {
  #     name = "gtk2";
  #   };
  # };

  # Hyprland-specific cursor fixes (since you're using Hyprland)
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "gsettings set org.gnome.desktop.interface cursor-theme 'Nordzy-cursors-white'"
      "gsettings set org.gnome.desktop.interface cursor-size 24"
    ];
  };

  stylix = {
    enable = true;

    autoEnable = true;
    polarity = "dark";

    targets = {
      firefox = {
        enable = true;
        profileNames = [
          "default"
        ];
      };
      wofi.enable = true;
      kitty.enable = false; # You have manual config
      waybar.enable = false; # You have manual config
      emacs.enable = false; # Using Doom themes
    };

    # Styling with Nowhere Colors
    base16Scheme = {
      base00 = "1a1d21"; # bg
      base01 = "22262b"; # bg-alt
      base02 = "282c34"; # base3
      base03 = "3d424a"; # base4
      base04 = "c0bdb8"; # fg-alt
      base05 = "e0dcd4"; # fg
      base06 = "e8e4dc"; # base8
      base07 = "f5f2eb"; # bright
      base08 = "c8beb8"; # red
      base09 = "c8c0b0"; # orange
      base0A = "ccc4b0"; # yellow
      base0B = "b4beb4"; # green
      base0C = "b0bcc8"; # cyan
      base0D = "b4bec8"; # blue
      base0E = "c4beb8"; # violet
      base0F = "c0b8bc"; # magenta
    };
  };
}
