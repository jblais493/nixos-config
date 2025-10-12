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
        folders = {
          # Example:
          # "phone-photos" = {
          #   path = "${cfg.mediaDir}/photos/phone";
          #   devices = [ "phone" ];
          # };
        };
      };
    };

    # Miniflux - RSS feed reader
    services.miniflux = {
      enable = true;
      config = {
        LISTEN_ADDR = "0.0.0.0:8082";
      };
      adminCredentialsFile = "/run/agenix/miniflux-admin";
    };

    # Paperless-NGX - Document management
    services.paperless = {
      enable = true;
      address = "0.0.0.0";
      port = 28981;
      settings = {
        PAPERLESS_OCR_LANGUAGE = "eng";
        PAPERLESS_TIME_ZONE = cfg.timezone;
        PAPERLESS_ADMIN_USER = cfg.user;
        PAPERLESS_ADMIN_PASSWORD = "changeme"; # Change this after first login
      };
    };

    # Immich - Self-hosted photo and video management
    services.immich = {
      enable = true;
      host = "0.0.0.0";
      port = 2283;
    };

    # Uptime Kuma - Service monitoring
    services.uptime-kuma = {
      enable = true;
      settings = {
        PORT = "3002";
        HOST = "0.0.0.0";
      };
    };

    # Microbin - Self-hosted pastebin
    services.microbin = {
      enable = true;
      settings = {
        MICROBIN_PORT = 8090;
        MICROBIN_BIND = "0.0.0.0";
        MICROBIN_PUBLIC_PATH = "https://paste.empirica";
        MICROBIN_EDITABLE = true;
        MICROBIN_HIDE_FOOTER = false;
        MICROBIN_PRIVATE = false;
      };
    };

    # Audiobookshelf
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

    services.bazarr = {
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

    # Homepage setup
    services.homepage-dashboard = {
      enable = true;
      listenPort = 3000;
      openFirewall = false;

      settings = {
        title = "Empirica Homelab";
        theme = "dark";
        color = "slate";
        background = "https://raw.githubusercontent.com/jblais493/Wallpapers/refs/heads/master/serenity.jpg";
        backgroundOpacity = 0.5;
        headerStyle = "clean";
        hideVersion = true;

        layout = {
          "Network Infrastructure" = {
            style = "row";
            columns = 3;
          };
          "Media Consumption" = {
            style = "row";
            columns = 3;
          };
          "Media Automation" = {
            style = "row";
            columns = 3;
          };
          "Knowledge & Documents" = {
            style = "row";
            columns = 3;
          };
          "Operations & Utilities" = {
            style = "row";
            columns = 3;
          };
        };
      };

      widgets = [
        {
          resources = {
            cpu = true;
            cputemp = true;
            memory = true;
            uptime = true;
          };
        }
        {
          greeting = {
            text_size = "sm";
            text = "Welcome Joshua";
          };
        }
        {
          datetime = {
            text_size = "sm";
            format = {
              dateStyle = "full";
            };
          };
        }
        {
          openmeteo = {
            label = "Edmonton";
            latitude = "53.5462";
            longitude = "-113.4937";
            units = "metric";
          };
        }
        # {
        #   search = {
        #     provider = "custom";
        #     url = "https://search.rhscz.eu/search?q=";
        #     target = "_blank";
        #     showSearchSuggestions = "true";
        #     focur = "true";
        #   };
        # }
      ];

      services = [
        # Foundation Layer: Infrastructure that enables everything else
        {
          "Network Infrastructure" = [
            {
              "AdGuard Home" = {
                icon = "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/webp/adguard-home.webp";
                href = "https://adguard.empirica";
                description = "DNS sovereignty and network-level blocking";
                # widget = {
                #   type = "adguard";
                #   url = "http://localhost:3001";
                # };
              };
            }
            {
              "Router" = {
                icon = "router";
                href = "http://192.168.0.1";
                description = "Gateway and perimeter defense";
              };
            }
            {
              "Syncthing" = {
                icon = "syncthing";
                href = "https://sync.empirica";
                description = "Distributed file synchronization";
                # widget = {
                #   type = "syncthing";
                #   url = "http://localhost:8384";
                # };
              };
            }
          ];
        }

        # Consumption Layer: Direct interaction with media
        {
          "Media Consumption" = [
            {
              "Jellyfin" = {
                icon = "jellyfin";
                href = "https://jellyfin.empirica";
                description = "Self-hosted media server";
                # widget = {
                #   type = "jellyfin";
                #   url = "http://localhost:8096";
                #   key = "{{HOMEPAGE_VAR_JELLYFIN_KEY}}";
                # };
              };
            }
            {
              "Audiobookshelf" = {
                icon = "audiobookshelf";
                href = "https://audiobookshelf.empirica";
                description = "Audiobook and podcast server";
              };
            }
            {
              "Calibre" = {
                icon = "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/png/calibre-web.png";
                href = "https://calibre.empirica";
                description = "Ebook library and management";
              };
            }
          ];
        }

        # Automation Layer: The invisible machinery (your "arr" stack)
        {
          "Media Automation" = [
            {
              "Prowlarr" = {
                icon = "prowlarr";
                href = "https://prowlarr.empirica";
                description = "Indexer manager - the source of all sources";
                # widget = {
                #   type = "prowlarr";
                #   url = "http://localhost:9696";
                #   key = "{{HOMEPAGE_VAR_PROWLARR_KEY}}";
                # };
              };
            }
            {
              "Radarr" = {
                icon = "radarr";
                href = "https://radarr.empirica";
                description = "Movie collection manager";
                # widget = {
                #   type = "radarr";
                #   url = "http://localhost:7878";
                #   key = "{{HOMEPAGE_VAR_RADARR_KEY}}";
                # };
              };
            }
            {
              "Sonarr" = {
                icon = "sonarr";
                href = "https://sonarr.empirica";
                description = "Series collection manager";
                # widget = {
                #   type = "sonarr";
                #   url = "http://localhost:8989";
                #   key = "{{HOMEPAGE_VAR_SONARR_KEY}}";
                # };
              };
            }
            {
              "Lidarr" = {
                icon = "lidarr";
                href = "https://lidarr.empirica";
                description = "Music collection manager";
                # widget = {
                #   type = "lidarr";
                #   url = "http://localhost:8686";
                #   key = "{{HOMEPAGE_VAR_LIDARR_KEY}}";
                # };
              };
            }
            {
              "Bazarr" = {
                icon = "bazarr";
                href = "https://bazarr.empirica";
                description = "Subtitle automation";
                # widget = {
                #   type = "bazarr";
                #   url = "http://localhost:6767";
                #   key = "{{HOMEPAGE_VAR_BAZARR_KEY}}";
                # };
              };
            }
            {
              "SABnzbd" = {
                icon = "sabnzbd";
                href = "https://sabnzbd.empirica";
                description = "Usenet download client";
                # widget = {
                #   type = "sabnzbd";
                #   url = "http://localhost:8080";
                #   key = "{{HOMEPAGE_VAR_SABNZBD_KEY}}";
                # };
              };
            }
          ];
        }

        # Knowledge Management: Information consumption and document storage
        {
          "Knowledge & Documents" = [
            {
              "Miniflux" = {
                icon = "miniflux";
                href = "https://miniflux.empirica";
                description = "RSS feed reader - curated information diet";
              };
            }
            {
              "Paperless-NGX" = {
                icon = "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/webp/paperless-ngx.webp";
                href = "https://paperless.empirica";
                description = "Document management with OCR";
                # widget = {
                #   type = "paperlessngx";
                #   url = "http://localhost:28981";
                #   key = "{{HOMEPAGE_VAR_PAPERLESS_KEY}}";
                # };
              };
            }
            {
              "Immich" = {
                icon = "immich";
                href = "https://immich.empirica";
                description = "Self-hosted photo and video management";
              };
            }
          ];
        }

        # Operations: Monitoring and utilities
        {
          "Operations & Utilities" = [
            {
              "Uptime Kuma" = {
                icon = "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/webp/uptime-kuma.webp";
                href = "https://uptime.empirica";
                description = "Service monitoring and status";
                # widget = {
                #   type = "uptimekuma";
                #   url = "http://localhost:3002";
                #   slug = "homelab";
                # };
              };
            }
            {
              "Microbin" = {
                icon = "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/webp/microbin.webp";
                href = "https://paste.empirica";
                description = "Self-hosted pastebin";
              };
            }
          ];
        }
      ];

      # When ready, uncomment this and create /run/secrets/homepage-env with API keys
      # environmentFile = "/run/secrets/homepage-env";
    };

    # Homepage environment configuration
    # NOTE: Required because homepage-dashboard module doesn't expose environment config
    systemd.services.homepage-dashboard.serviceConfig = {
      Environment = [
        "HOMEPAGE_ALLOWED_HOSTS=homepage.empirica,100.69.46.98:3000,100.69.46.98"
      ];
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
              header_up Host localhost:8384
              header_up X-Forwarded-Host {host}
            }
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
        "bazarr.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:6767
          '';
        };
        "sabnzbd.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:8080
          '';
        };
        # New services
        "miniflux.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:8082
          '';
        };
        "paperless.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:28981
          '';
        };
        "immich.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:2283 {
              header_up X-Forwarded-For {remote_host}
              header_up X-Forwarded-Proto {scheme}
            }
          '';
        };
        "uptime.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:3002
          '';
        };
        "paste.empirica" = {
          extraConfig = ''
            tls internal
            reverse_proxy localhost:8090
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
      "d '${cfg.mediaDir}/photos' 0775 joshua joshua - -"  # Changed: joshua:joshua, mode 0775 for immich use

      # Media folders for immich
      "d /var/lib/immich 0750 immich immich -"
      "d /var/lib/immich/upload 0750 immich immich -"
      "d /var/lib/immich/library 0750 immich immich -"

      # Container config directories (only for Calibre now)
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

    # Calibre-Web-Automated container (only container needed)
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
        PUID = "1000";
        PGID = "100";
        TZ = cfg.timezone;
        CALIBRE_LIBRARY_PATH = "/books";
        METADATA_UPDATE = "true";
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
