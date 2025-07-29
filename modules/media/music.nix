{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Music players
    mpd
    mpc
    ncmpcpp
    spotifyd
    spotdl
    # Audio tools
    audacity
    # Audio codecs
    ffmpeg-full
    flac
    lame
  ];

  # Add the MPD service configuration
  services.mpd = {
    enable = true;
    user = "joshua";  # Replace with your username
    group = "users";
    musicDirectory = "/home/joshua/MusicOrganized/";
    extraConfig = ''
      playlist_directory "/home/joshua/.config/mpd/playlists"
      db_file "/home/joshua/.config/mpd/database"
      log_file "/home/joshua/.config/mpd/log"
      pid_file "/home/joshua/.config/mpd/pid"
      state_file "/home/joshua/.config/mpd/state"
      sticker_file "/home/joshua/.config/mpd/sticker.sql"

      bind_to_address "localhost"
      port "6600"
      auto_update "yes"
      metadata_to_use "+comment"

      # PipeWire-compatible audio output
      audio_output {
          type "pipewire"
          name "PipeWire Audio"
      }

      # Fallback to pulse (pipewire provides pulse compatibility)
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

  # Ensure users can access audio devices
  users.groups.audio = {};
}
