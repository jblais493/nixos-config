{
  config,
  pkgs,
  lib,
  ...
}:

let
  distractionDomains = [
    # Social media
    "reddit.com"
    "old.reddit.com"
    "new.reddit.com"
    "www.reddit.com"
    "twitter.com"
    "x.com"
    "www.twitter.com"
    # "youtube.com"
    # "www.youtube.com"
    # "m.youtube.com"
    # "youtu.be"
    # "studio.youtube.com"
    "facebook.com"
    "www.facebook.com"
    "instagram.com"
    "www.instagram.com"
    "tiktok.com"
    "www.tiktok.com"
    "linkedin.com"
    "www.linkedin.com"

    # Games
    "chess.com"
    "www.chess.com"
    "lichess.org"
    "www.lichess.org"

    # Tech news/aggregators
    "news.ycombinator.com"
    "lobste.rs"
    "slashdot.org"
    "techcrunch.com"
    "www.techcrunch.com"
    "arstechnica.com"
    "www.arstechnica.com"
    "theverge.com"
    "www.theverge.com"
    "wired.com"
    "www.wired.com"
    "engadget.com"
    "www.engadget.com"

    # Streaming
    "netflix.com"
    "www.netflix.com"
    "twitch.tv"
    "www.twitch.tv"
    "hulu.com"
    "www.hulu.com"
    "disneyplus.com"
    "www.disneyplus.com"

    # Major news - International
    "cbc.ca"
    "www.cbc.ca"
    "bbc.com"
    "www.bbc.com"
    "bbc.co.uk"
    "www.bbc.co.uk"
    "cnn.com"
    "www.cnn.com"
    "ctvnews.ca"
    "www.ctvnews.ca"
    "edmontonjournal.com"
    "www.edmontonjournal.com"
    "foxnews.com"
    "www.foxnews.com"
    "msnbc.com"
    "www.msnbc.com"
    "nbcnews.com"
    "www.nbcnews.com"
    "abcnews.go.com"
    "cbsnews.com"
    "www.cbsnews.com"

    # Newspapers - US/Canada
    "nytimes.com"
    "www.nytimes.com"
    "washingtonpost.com"
    "www.washingtonpost.com"
    "wsj.com"
    "www.wsj.com"
    "usatoday.com"
    "www.usatoday.com"
    "latimes.com"
    "www.latimes.com"
    "theglobeandmail.com"
    "www.theglobeandmail.com"
    "nationalpost.com"
    "www.nationalpost.com"

    # Newspapers - UK
    "theguardian.com"
    "www.theguardian.com"
    "guardian.co.uk"
    "www.guardian.co.uk"
    "telegraph.co.uk"
    "www.telegraph.co.uk"
    "independent.co.uk"
    "www.independent.co.uk"
    "thetimes.co.uk"
    "www.timetimes.co.uk"
    "dailymail.co.uk"
    "www.dailymail.co.uk"

    # News aggregators
    "news.google.com"
    "google.com/news"
    "www.google.com/news"
    "flipboard.com"
    "www.flipboard.com"
    "apple.com/news"
    "www.apple.com/news"

    # Business/Financial
    "bloomberg.com"
    "www.bloomberg.com"
    "reuters.com"
    "www.reuters.com"
    "ft.com"
    "www.ft.com"
    "forbes.com"
    "www.forbes.com"
    "cnbc.com"
    "www.cnbc.com"
    "marketwatch.com"
    "www.marketwatch.com"

    # Magazines
    "theatlantic.com"
    "www.theatlantic.com"
    "newyorker.com"
    "www.newyorker.com"
    "time.com"
    "www.time.com"
    "politico.com"
    "www.politico.com"
  ];

  workHoursStart = 5;
  workHoursEnd = 19;

  unboundBlockConfig = lib.concatMapStringsSep "\n" (domain: ''
    local-zone: "${domain}" redirect
    local-data: "${domain} A 127.0.0.1"
  '') distractionDomains;

in
{
  environment.etc."unbound/blocked-domains.conf".text = unboundBlockConfig;

  # Disable systemd-resolved stub listener
  services.resolved = {
    enable = true;
    dnssec = "false";
    extraConfig = ''
      DNSStubListener=no
    '';
  };

  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [
          "127.0.0.1"
          "::1"
        ];
        port = 53;
        access-control = [
          "127.0.0.0/8 allow"
          "::1/128 allow"
        ];
        do-not-query-localhost = false;
        verbosity = 1;

        # CRITICAL: Disable DNSSEC to avoid root key requirement
        module-config = ''"iterator"'';

        include = [ "/etc/unbound/blocked-domains.conf" ];
      };

      forward-zone = [
        {
          name = ".";
          forward-addr = [
            "1.1.1.1@53"
            "1.0.0.1@53"
          ];
        }
      ];
    };
  };

  systemd.services.distraction-blocker = {
    description = "Time-aware distraction blocker via DNS";
    after = [ "network.target" ];
    # Remove wantedBy - only timer should trigger this

    serviceConfig = {
      Type = "oneshot";
      # Remove RemainAfterExit - let it finish
      ExecStart = pkgs.writeShellScript "toggle-blocking" ''
        set -euo pipefail

        current_hour=$(${pkgs.coreutils}/bin/date +%H)

        if [ "$current_hour" -ge ${toString workHoursStart} ] && [ "$current_hour" -lt ${toString workHoursEnd} ]; then
          ${pkgs.systemd}/bin/systemctl start unbound.service || true
          echo "Distraction blocking ENABLED (work hours: $current_hour)"
        else
          ${pkgs.systemd}/bin/systemctl stop unbound.service || true
          echo "Distraction blocking DISABLED (off hours: $current_hour)"
        fi
      '';
    };
  };

  systemd.timers.distraction-blocker = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "hourly";
      OnBootSec = "1min"; # Check state on boot
      Persistent = true;
    };
  };

  networking.nameservers = lib.mkForce [ "127.0.0.1" ];
}
