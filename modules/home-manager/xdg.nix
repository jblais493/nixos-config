{ config, ... }: # Add 'config' here
{
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
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
        "libreoffice-writer.desktop";
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "libreoffice-calc.desktop";
      "x-scheme-handler/mailto" = "emacsclient-mail.desktop";
    };
  };

  # Create wrapper script for proper quoting
  home.file.".local/bin/emacs-mailto".text = ''
    #!/usr/bin/env bash
    # Extract mailto URL
    url="$1"
    # Pass to emacsclient with proper quoting
    emacsclient -c -a "" -n --eval "(mu4e-compose-mailto \"$url\")"
  '';

  home.file.".local/bin/emacs-mailto".executable = true;

  # Update desktop entry to use wrapper
  xdg.dataFile."applications/emacsclient-mail.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=Emacs (Client) Mail
    GenericName=Text Editor
    Comment=Compose email with mu4e
    MimeType=x-scheme-handler/mailto;
    Exec=${config.home.homeDirectory}/.local/bin/emacs-mailto %u
    Icon=emacs
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;Email;
  '';
}
