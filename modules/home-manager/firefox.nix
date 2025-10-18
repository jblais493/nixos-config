# modules/home-manager/browsers.nix
{ config, pkgs, ... }:
{
  programs.firefox = {
    enable = true;

    # System-level policies (works in home-manager too)
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      BlockAboutConfig = false;
      OfferToSaveLogins = false;
    };

    # Native messaging for Tridactyl
    nativeMessagingHosts = [ pkgs.tridactyl-native ];

    profiles.default = {
      name = "Default";
      isDefault = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        tridactyl
        don-t-fuck-with-paste
        decentraleyes
        privacy-badger
        istilldontcareaboutcookies
        violentmonkey
        wayback-machine
        leechblock-ng
        kristofferhagen-nord-theme
        libredirect
      ];

      settings = {
        # Privacy hardening
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.donottrackheader.enabled" = true;
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "toolkit.telemetry.enabled" = false;
        "dom.security.https_only_mode" = true;
        "security.tls.insecure_fallback_hosts" = "";
        "media.autoplay.default" = 5;
        "browser.cache.disk.enable" = false;
        "browser.sessionstore.privacy_level" = 2;

        # Hyprland/Wayland optimization
        "widget.use-xdg-desktop-portal.file-picker" = 1;
        "gfx.webrender.all" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "layers.acceleration.force-enabled" = true;
      };

      userChrome = ''
        #TabsToolbar {
          visibility: collapse !important;
        }

        :root {
          --nord0: #2e3440;
          --nord1: #3b4252;
          --nord2: #434c5e;
          --nord3: #4c566a;
          --nord4: #d8dee9;
          --nord5: #e5e9f0;
          --nord6: #eceff4;
          --nord7: #8fbcbb;
          --nord8: #88c0d0;
          --nord9: #81a1c1;
          --nord10: #5e81ac;
        }

        #navigator-toolbox {
          background-color: var(--nord1) !important;
        }

        #urlbar {
          background-color: var(--nord2) !important;
          color: var(--nord6) !important;
        }
      '';

      bookmarks = {
        force = true;
        settings = [
          {
            name = "Toolbar";
            toolbar = true;
            bookmarks = [
              {
                name = "NixOS Manual";
                url = "https://nixos.org/manual/nixos/stable/";
              }
            ];
          }
        ];
      };
    };
  };
}
