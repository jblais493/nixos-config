{ config, pkgs, ... }:
{
  programs.firefox = {
    enable = true;

    profiles.default = {
      name = "Default";
      isDefault = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        tridactyl
        dont-fuck-with-paste
        decentraleyes
        privacy-badger
        i-still-dont-care-about-cookies
        violentmonkey
        wayback-machine
        leechblock-ng
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

      bookmarks = [
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

  # Brave and Zen browser don't have home-manager modules yet
  # They're installed system-wide via the desktop/browsers.nix module
}
