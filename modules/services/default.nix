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
      openFirewall = false;
    };

    # Syncthing - File synchronization
    services.syncthing = {
      enable = true;
      user = cfg.user;
      dataDir = "/home/${cfg.user}/syncthing";
      configDir = "/home/${cfg.user}/.config/syncthing";
      openDefaultPorts = false;
    };

    # The *arr stack for media management
    services.radarr = {
      enable = true;
      openFirewall = false;
    };

    services.lidarr = {
      enable = true;
      openFirewall = false;
    };

    services.prowlarr = {
      enable = true;
      openFirewall = false;
    };

    services.sonarr = {
      enable = true;
      openFirewall = false;
    };

    # Calibre-web for ebook management
    services.calibre-web = {
      enable = true;
      listen.port = 8083;
      openFirewall = false;
      options = {
        calibreLibrary = "${cfg.mediaDir}/books";
        enableBookUploading = true;
      };
    };

    # Radicale - CalDAV/CardDAV server
    services.radicale = {
      enable = true;
      settings = {
        server = {
          hosts = [ "127.0.0.1:5232" ];  # Actual localhost binding
        };
        auth = {
          type = "htpasswd";
          htpasswd_filename = "/var/lib/radicale/users";
          htpasswd_encryption = "bcrypt";
        };
        storage = {
          filesystem_folder = "/var/lib/radicale/collections";
        };
        logging = {
          level = "info";
        };
      };
    };

    # Reverse proxy for all services (critical for clean architecture)
    services.caddy = {
      enable = true;
      virtualHosts = {
        "jellyfin.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:8096
          '';
        };
        "homepage.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:3000
          '';
        };
        "radicale.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:5232
          '';
        };
        "pihole.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:8080
          '';
        };
        "audiobookshelf.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:13378
          '';
        };
        "calibre.empirica.local" = {
          extraConfig = ''
            reverse_proxy localhost:8083
          '';
        };
      };
    };

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

    # Podman for containers
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };

    # Container services
    systemd.services = {
      # Audiobookshelf
      audiobookshelf = {
        description = "Audiobookshelf";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Restart = "always";
          ExecStart = "${pkgs.podman}/bin/podman run --rm --name audiobookshelf " +
            "-p 127.0.0.1:13378:80 " +
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
            "-p 127.0.0.1:3000:3000 " +
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
          User = "root";
          Restart = "unless-stopped";
          ExecStart = "${pkgs.podman}/bin/podman run --rm --name pihole " +
            "-p 127.0.0.1:53:53/tcp -p 127.0.0.1:53:53/udp -p 127.0.0.1:8080:80 " +
            "-e TZ=${cfg.timezone} " +
            "-e WEBPASSWORD=changeme " +
            "-v /home/${cfg.user}/containers/pihole/etc-pihole:/etc/pihole " +
            "-v /home/${cfg.user}/containers/pihole/etc-dnsmasq.d:/etc/dnsmasq.d " +
            "docker.io/pihole/pihole:latest";
          ExecStop = "${pkgs.podman}/bin/podman stop pihole";
        };
      };
    };

    # Tailscale service
    services.tailscale.enable = true;

    # Firewall: Only Caddy (HTTP/HTTPS) accessible via Tailscale
    networking.firewall = {
      interfaces."tailscale0" = {
        allowedTCPPorts = [ 80 443 ];  # Only Caddy - it proxies everything
      };
      # Everything else locked down
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };
}
