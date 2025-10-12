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

  # Syncthing for data sync - daily to server + phone to server
services.syncthing = {
      enable = true;
      user = cfg.user;
      dataDir = "/home/${cfg.user}/syncthing";
      configDir = "/home/${cfg.user}/.config/syncthing";
      openDefaultPorts = false;
      overrideDevices = true;
      overrideFolders = true;

      settings = {
        devices = {
          "phone" = {
            id = "TTUKVRU-FEJGUXM-SERMOTN-TJNRQKV-7QP2N5J-V3ESDBE-5WTKB4K-2LCGDA3";
          };
        };
        # Add folders here when you're ready
        folders = {
          # Example:
          # "phone-photos" = {
          #   path = "${cfg.mediaDir}/photos/phone";
          #   devices = [ "phone" ];
          # };
        };
      };
    };


# Homepage setup:
services.homepage-dashboard = {
  enable = true;
  listenPort = 3000;
  openFirewall = false;

  settings = {
    title = "Empirica Homelab";
  };

  services = [
    {
      "Network Infrastructure" = [
        {
          "Router" = {
            icon = "mdi-router-wireless";
            href = "http://192.168.0.1";
            description = "Network gateway and firewall";
          };
        }
        {
          "AdGuard Home" = {
            icon = "mdi-shield-check";
            href = "https://adguard.empirica";
            description = "DNS and ad blocking";
          };
        }
        {
          "Syncthing" = {
            icon = "mdi-sync";
            href = "https://sync.empirica";
            description = "File synchronization";
            widget = {
              type = "syncthing";
              url = "http://localhost:8384";
              # Optional: add API key for stats
              # key = "your-syncthing-api-key";
            };
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
            description = "Media playback";
          };
        }
        {
          "Audiobookshelf" = {
            icon = "mdi-book-music";
            href = "https://audiobookshelf.empirica";
            description = "Audiobooks";
          };
        }
        {
          "Calibre" = {
            icon = "mdi-book-open-variant";
            href = "https://calibre.empirica";
            description = "Ebook library";
          };
        }
      ];
    }
  ];
};

# Add the environment variable
systemd.services.homepage-dashboard = {
  serviceConfig = {
    Environment = [ "HOMEPAGE_ALLOWED_HOSTS=homepage.empirica,100.69.46.98:3000,100.69.46.98" ];
  };
};

    services.audiobookshelf = {
      enable = true;
      port = 13378;
      host = "0.0.0.0";
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

    services.sabnzbd = {
      enable = true;
      openFirewall = false;
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
          hosts = [ "0.0.0.0:5232" ];
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
"sync.empirica" = {
  extraConfig = ''
    tls internal
    reverse_proxy localhost:8384 {
      # Forward original host header
      header_up Host localhost:8384
      header_up X-Forwarded-Host {host}
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
     "radarr.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:7878
      '';
    };
    "sonarr.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:8989
      '';
    };
    "lidarr.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:8686
      '';
    };
    "prowlarr.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:9696
      '';
    };
    "sabnzbd.empirica" = {
      extraConfig = ''
        tls internal
        reverse_proxy localhost:8080
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
      "d '/home/${cfg.user}/containers/calibre-web-automated' 0755 ${cfg.user} users - -"
      "d '/home/${cfg.user}/containers/calibre-web-automated/config' 0755 ${cfg.user} users - -"

      # Radicale directory and empty users file
      "d '/var/lib/radicale' 0750 radicale radicale - -"
      "f '/var/lib/radicale/users' 0640 radicale radicale - -"
    ];

    # Podman for containers
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = false;
    };

# Add Calibre-Web-Automated as a container
virtualisation.oci-containers.containers.calibre-web-automated = {
  image = "crocodilestick/calibre-web-automated:latest";
  autoStart = true;

  ports = [
    "0.0.0.0:8083:8083"
  ];

  volumes = [
    "${cfg.mediaDir}/books:/books"
    "/home/${cfg.user}/containers/calibre-web-automated/config:/config"
  ];

  environment = {
    PUID = "1000";  # Your user ID
    PGID = "100";   # users group
    TZ = cfg.timezone;

    # Calibre-Web-Automated specific settings
    CALIBRE_LIBRARY_PATH = "/books";

    # Optional: Enable automatic metadata fetching
    METADATA_UPDATE = "true";

    # Optional: Goodreads/Google Books API keys for better metadata
    # GOODREADS_API_KEY = "your-key-here";
    # GOOGLE_BOOKS_API_KEY = "your-key-here";
  };
};

    # Tailscale service
    services.tailscale.enable = true;

    # Firewall: Allow access on LAN
    networking.firewall = {
      allowedTCPPorts = [ 53 80 443 3001 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
