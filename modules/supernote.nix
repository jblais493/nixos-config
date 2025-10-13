# ../../modules/supernote/default.nix
{ config, pkgs, lib, ... }:

let
  cfg = config.services.supernote-watcher;

  supernote-tools = pkgs.callPackage (builtins.fetchGit {
    url = "https://github.com/jblais493/supernote";
    ref = "main";
  }) {};

  username = "joshua";  # Your username
in
{
  options.services.supernote-watcher = {
    enable = lib.mkEnableOption "Supernote automatic PDF conversion watcher";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ supernote-tools ];

    # Create the service for your specific user
    systemd.services."supernote-watcher@${username}" = {
      description = "Supernote automatic PDF conversion for ${username}";
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
