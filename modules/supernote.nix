# modules/supernote.nix
{ config, pkgs, lib, inputs, ... }:

let
  cfg = config.services.supernote-watcher;
  supernote-tools = inputs.supernote-tools.packages.${pkgs.system}.default;
  username = "joshua";
in
{
  options.services.supernote-watcher = {
    enable = lib.mkEnableOption "Supernote automatic PDF conversion watcher";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ supernote-tools ];

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
