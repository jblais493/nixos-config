{ config, pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    profiles.default = {
      name = "Default";
      isDefault = true;

      # Updated extension syntax
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
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
        sidebery
      ];

      settings = {
        # Privacy settings
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.donottrackheader.enabled" = true;

        # Disable telemetry
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "toolkit.telemetry.enabled" = false;

        # Security
        "dom.security.https_only_mode" = true;
        "security.tls.insecure_fallback_hosts" = "";

        # Disable autoplay
        "media.autoplay.default" = 5;

        # Performance
        "browser.cache.disk.enable" = false;
        "browser.sessionstore.privacy_level" = 2;
      };

      # Custom userChrome.css for vertical tabs
      userChrome = ''
        /* Hide horizontal tabs when using Tree Style Tab */
        #TabsToolbar {
          visibility: collapse !important;
        }

        /* Nord color scheme for Firefox UI */
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

        /* Apply Nord colors to browser chrome */
        #navigator-toolbox {
          background-color: var(--nord1) !important;
        }

        #urlbar {
          background-color: var(--nord2) !important;
          color: var(--nord6) !important;
        }
      '';

      # Updated bookmarks syntax
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
              # Add your bookmarks here
            ];
          }
        ];
      };
    };
  };

  # Brave and Zen browser don't have home-manager modules yet
  # They're installed system-wide via the desktop/browsers.nix module
}
