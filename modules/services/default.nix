{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homelab;
in
{
  options.services.homelab = {
    enable = mkEnableOption "Enable all homelab services";

    mediaDir = mkOption {
      type = types.str;
      default = "/home/joshua/Media";
      description = "Base directory for media files";
    };

    user = mkOption {
      type = types.str;
      default = "joshua";
      description = "Main user for services";
    };

    timezone = mkOption {
      type = types.str;
      default = "America/Edmonton";
      description = "Timezone for services";
    };
  };

  config = mkIf cfg.enable {
    # Jellyfin - Native media server
    services.jellyfin = {
      enable = true;
      openFirewall = true;
      # Jellyfin will auto-detect media in common locations
    };

    # Syncthing - File synchronization
    services.syncthing = {
      enable = true;
      user = cfg.user;
      dataDir = "/home/${cfg.user}/syncthing";
      configDir = "/home/${cfg.user}/.config/syncthing";
      openDefaultPorts = true;
    };

    # The *arr stack for media management
    services.radarr = {
      enable = true;
      openFirewall = true;
    };

    services.lidarr = {
      enable = true;
      openFirewall = true;
    };

    services.prowlarr = {
      enable = true;
      openFirewall = true;
    };

    services.sonarr = {
      enable = true;
      openFirewall = true;
    };

    # Calibre-web for ebook management
    services.calibre-web = {
      enable = true;
      listen.port = 8083;
      openFirewall = true;
      options = {
        calibreLibrary = "${cfg.mediaDir}/books";
        enableBookUploading = true;
      };
    };

    # Nextcloud for cloud storage and collaboration
    services.nextcloud = {
      enable = true;
      hostName = "localhost"; # Change this to your domain
      database.createLocally = true;
      configureRedis = true;
      maxUploadSize = "16G";
      https = false; # Set to true when you have proper domain/certs
      config = {
        adminuser = "admin";
        adminpassFile = "/etc/nixos/nextcloud-admin-pass";
      };
      settings = {
        default_phone_region = "CA";
        maintenance_window_start = 2;
      };
    };

    # Create the admin password file (you'll need to create this)
    environment.etc."nextcloud-admin-pass" = {
      text = "change-this-password";
      mode = "0600";
      user = "nextcloud";
      group = "nextcloud";
    };

    # Plex (alternative to Jellyfin - enable one or the other)
    # services.plex = {
    #   enable = true;
    #   openFirewall = true;
    #   dataDir = "/var/lib/plex";
    # };

    # Create media directories with proper permissions
    systemd.tmpfiles.rules = [
      "d '${cfg.mediaDir}' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/movies' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/tvshows' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/music' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/books' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/audiobooks' 0755 ${cfg.user} users - -"
      "d '${cfg.mediaDir}/photos' 0755 ${cfg.user} users - -"
    ];

    # Services that might need container approach (for now)
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };

    # Container services for things without native NixOS support
    systemd.services = {
      # Audiobookshelf
      audiobookshelf = {
        description = "Audiobookshelf";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Restart = "unless-stopped";
          ExecStart = "${pkgs.podman}/bin/podman run --rm --name audiobookshelf " +
            "-p 13378:80 " +
            "-e TZ=${cfg.timezone} " +
            "-v /home/${cfg.user}/containers/audiobookshelf/config:/config " +
            "-v /home/${cfg.user}/containers/audiobookshelf/metadata:/metadata " +
            "-v ${cfg.mediaDir}/audiobooks:/audiobooks " +
            "ghcr.io/advplyr/audiobookshelf:latest";
          ExecStop = "${pkgs.podman}/bin/podman stop audiobookshelf";
        };
      };

      # Homepage dashboard
      homepage = {
        description = "Homepage Dashboard";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Restart = "unless-stopped";
          ExecStart = "${pkgs.podman}/bin/podman run --rm --name homepage " +
            "-p 3000:3000 " +
            "-e PUID=1000 -e PGID=1000 " +
            "-v /home/${cfg.user}/containers/homepage/config:/app/config " +
            "ghcr.io/gethomepage/homepage:latest";
          ExecStop = "${pkgs.podman}/bin/podman stop homepage";
        };
      };

      # Pi-hole
      pihole = {
        description = "Pi-hole DNS";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "exec";
          User = "root"; # Pi-hole needs root for DNS
          Restart = "unless-stopped";
          ExecStart = "${pkgs.podman}/bin/podman run --rm --name pihole " +
            "-p 53:53/tcp -p 53:53/udp -p 8080:80 " +
            "-e TZ=${cfg.timezone} " +
            "-e WEBPASSWORD=changeme " +
            "-v /home/${cfg.user}/containers/pihole/etc-pihole:/etc/pihole " +
            "-v /home/${cfg.user}/containers/pihole/etc-dnsmasq.d:/etc/dnsmasq.d " +
            "docker.io/pihole/pihole:latest";
          ExecStop = "${pkgs.podman}/bin/podman stop pihole";
        };
      };
    };

    # Firewall ports for container services
    networking.firewall = {
      allowedTCPPorts = [ 3000 8080 13378 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
