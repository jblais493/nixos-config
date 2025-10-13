{ config, pkgs, lib, ... }:

let
  cfg = config.services.supernote-watcher;

  supernote-tools = pkgs.callPackage (builtins.fetchGit {
    url = "https://github.com/jblais493/supernote";
    ref = "main";
  }) {};
in
{
  options.services.supernote-watcher = {
    enable = lib.mkEnableOption "Supernote automatic PDF conversion watcher";

    notesDir = lib.mkOption {
      type = lib.types.str;
      default = "$HOME/Documents/supernote";
      description = "Directory containing .note files";
    };

    pdfDir = lib.mkOption {
      type = lib.types.str;
      default = "$HOME/Documents/supernote-pdf";
      description = "Directory for converted PDF files";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ supernote-tools ];

    systemd.user.services.supernote-watcher = {
      description = "Supernote automatic PDF conversion";
      after = [ "graphical-session.target" ];
      wantedBy = [ "default.target" ];

      serviceConfig = {
        ExecStart = "${supernote-tools}/bin/supernote-watcher";
        Restart = "always";
        RestartSec = "10s";
      };
    };
  };
}
