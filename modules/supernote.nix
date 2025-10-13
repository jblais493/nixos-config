# modules/supernote.nix
{ config, pkgs, lib, ... }:

let
  cfg = config.services.supernote-watcher;

  supernote-tools = pkgs.callPackage (builtins.fetchGit {
    url = "https://github.com/jblais493/supernote";
    ref = "main";
  }) {};

  username = "joshua";
in
{
  options.services.supernote-watcher = {
    enable = lib.mkEnableOption "Supernote automatic PDF conversion watcher";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ supernote-tools ];

    # Create system service that runs as your user
    systemd.services.supernote-watcher-joshua = {
      description = "Supernote automatic PDF conversion for joshua";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        User = username;
        ExecStart = "${supernote-tools}/bin/supernote-watcher";
        Restart = "always";
        RestartSec = "10s";
      };
    };
  };
}
