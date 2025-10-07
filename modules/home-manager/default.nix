{ config, lib, pkgs, inputs, ... }:

let
  # Create a function to make out-of-store symlinks
  mkOutOfStoreSymlink = path: config.lib.file.mkOutOfStoreSymlink path;
in
{
  imports = [
    ./git.nix
    ./setup.nix
    ./theming.nix
    ./browsers.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

   # XDG MIME associations for file types
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "video/mp4" = "mpv.desktop";
      "video/x-msvideo" = "mpv.desktop";
      "video/x-matroska" = "mpv.desktop";
      "video/webm" = "mpv.desktop";
      "video/quicktime" = "mpv.desktop";
      "audio/mpeg" = "mpv.desktop";
      "audio/flac" = "mpv.desktop";
      "audio/ogg" = "mpv.desktop";
      "audio/wav" = "mpv.desktop";
      "image/jpeg" = "feh.desktop";
      "image/png" = "feh.desktop";
      "image/gif" = "feh.desktop";
      "application/pdf" = "org.pwmt.zathura.desktop";
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "libreoffice-writer.desktop";
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "libreoffice-calc.desktop";
    };
  };

  # Use mkOutOfStoreSymlink for live editing
  home.file = {
    ".config/doom".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/doom";
    ".zshrc".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/zsh/.zshrc";
    ".config/starship.toml".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/starship/starship.toml";
    ".config/tmux/plugins".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/tmux/plugins";
    ".config/hypr".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";
    ".config/kitty".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/kitty";
    ".config/waybar".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";
    ".config/swaync".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";
    ".config/wofi".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
    ".config/nvim".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/nvim";
    ".config/fastfetch".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/fastfetch";
    ".config/zathura".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/zathura";
    ".config/btop".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/btop";
    ".tmux.conf".source = mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-config/dotfiles/tmux/.tmux.conf";
  };

  # MPD Service
    services.mpd = {
    enable = true;
    musicDirectory = "/home/joshua/MusicOrganized";
    extraConfig = ''
      bind_to_address "localhost"
      port "6600"
      auto_update "yes"
      metadata_to_use "+comment"

      audio_output {
        type "pipewire"
        name "PipeWire Audio"
      }

      audio_output {
        type "pulse"
        name "Pulse Audio"
      }

      audio_output {
        type "fifo"
        name "album_art"
        path "/tmp/mpd.fifo"
        format "44100:16:2"
      }

      audio_output {
        type "httpd"
        name "HTTP Stream"
        encoder "vorbis"
        port "8000"
        bind_to_address "127.0.0.1"
        quality "5.0"
        format "44100:16:2"
      }
    '';
  };
}
