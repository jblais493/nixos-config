{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homelab;

  # For a server, it's most reliable to use its static IP directly.
  # Ensure your router has a DHCP reservation to always assign this IP.
  serverIP = "192.168.0.28";

  # Automatically generate DNS rewrite rules from Caddy's virtual hosts
  dnsRewrites = lib.mapAttrsToList (name: value: {
    domain = builtins.head (lib.splitString ":" name);
    answer = serverIP;
  }) config.services.caddy.virtualHosts;

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
    environment.systemPackages = with pkgs; [
      # needed for radicale
        apacheHttpd
    ];

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


# Homepage setup:
services.homepage-dashboard = {
  enable = true;
  listenPort = 3000;
  openFirewall = false;

  settings = {
    title = "Empirica Homelab";
    # Disable host validation or configure trusted proxies
    # disableHostValidation = true;  # Quick fix

    # Or better - explicitly trust Caddy
    trustedProxies = [ "127.0.0.1" "::1" ];
  };

  services = [
    {
      "Network Infrastructure" = [
        {
          "Router" = {
            icon = "mdi-router-wireless";
            href = "http://192.168.0.1";
            description = "Network gateway and firewall";
            siteMonitor = "http://192.168.0.1";
            statusStyle = "dot";
          };
        }
        {
          "AdGuard Home" = {
            icon = "mdi-shield-check";
            href = "https://adguard.empirica";
            description = "DNS and ad blocking";
            siteMonitor = "https://adguard.empirica";
          };
        }
        {
          "Syncthing" = {
            icon = "mdi-sync";
            href = "https://syncthing.empirica";  # If you add this to Caddy
            description = "File synchronization";
          };
        }
      ];
    }
    {
      "Media" = [
        {
          "Jellyfin" = {
            icon = "mdi-jellyfish-outline";
            href = "https://jellyfin.empirica";
            description = "Media playback for films and television";
            siteMonitor = "https://jellyfin.empirica";
          };
        }
        {
          "Audiobookshelf" = {
            icon = "mdi-book-music";
            href = "https://audiobookshelf.empirica";
            description = "Audiobook and podcast server";
          };
        }
        {
          "Calibre" = {
            icon = "mdi-book-open-variant";
            href = "https://calibre.empirica";
            description = "Ebook library management";
          };
        }
      ];
    }
  ];
};

    services.audiobookshelf = {
      enable = true;
      port = 13378;
      host = "127.0.0.1";  # Localhost only
      openFirewall = false;
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

services.adguardhome = {
      enable = true;
      host = "0.0.0.0";
      port = 3001;
      openFirewall = false;
      settings = {
        dns = {
          bind_hosts = [ "0.0.0.0" ];
          port = 53;
          upstream_dns = [
            "1.1.1.1"
            "8.8.8.8"
          ];
        };
        filtering = {
          rewrites = dnsRewrites;
        };
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
    "jellyfin.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:8096
      '';
    };
    "homepage.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:3000
      '';
    };
    "radicale.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:5232
      '';
    };
    "adguard.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:3001
      '';
    };
    "audiobookshelf.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:13378
      '';
    };
    "calibre.empirica" = {
      extraConfig = ''
        tls internal
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

      # Container config directories
      "d '/home/${cfg.user}/containers' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/audiobookshelf' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/audiobookshelf/config' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/audiobookshelf/metadata' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/homepage' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/homepage/config' 0755 ${cfg.user} users - -"

      # Radicale directory and empty users file
      "d '/var/lib/radicale' 0750 radicale radicale - -"
      "f '/var/lib/radicale/users' 0640 radicale radicale - -"
    ];

    # Podman for containers
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };

    # Container services
    # calibre-web-automated

    # Tailscale service
    services.tailscale.enable = true;

    # Firewall: Allow access on LAN
    networking.firewall = {
      allowedTCPPorts = [ 53 80 443 3001 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
