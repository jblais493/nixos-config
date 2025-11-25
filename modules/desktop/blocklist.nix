{
  config,
  pkgs,
  lib,
  ...
}:
{
  networking.hosts."127.0.0.1" = [
    # Social media
    "reddit.com"
    "old.reddit.com"
    "new.reddit.com"
    "www.reddit.com"
    "twitter.com"
    "x.com"
    "studio.youtube.com"
    "www.twitter.com"
    "youtube.com"
    "www.youtube.com"
    "m.youtube.com"
    "youtu.be"
    "facebook.com"
    "www.facebook.com"
    "instagram.com"
    "www.instagram.com"
    "tiktok.com"
    "www.tiktok.com"
    "linkedin.com"
    "www.linkedin.com"

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

    # Major news outlets - International
    "cbc.ca"
    "www.cbc.ca"
    "bbc.com"
    "www.bbc.com"
    "bbc.co.uk"
    "www.bbc.co.uk"
    "cnn.com"
    "www.cnn.com"
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
    "www.thetimes.co.uk"
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

    # Business/Financial news
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

    # Entertainment/Gossip
    "tmz.com"
    "www.tmz.com"
    "buzzfeed.com"
    "www.buzzfeed.com"
    "huffpost.com"
    "www.huffpost.com"
    "vice.com"
    "www.vice.com"

    # Sports (distraction vector)
    "espn.com"
    "www.espn.com"
    "sportsnet.ca"
    "www.sportsnet.ca"
    "tsn.ca"
    "www.tsn.ca"

    # Magazine sites
    "theatlantic.com"
    "www.theatlantic.com"
    "newyorker.com"
    "www.newyorker.com"
    "time.com"
    "www.time.com"
    "politico.com"
    "www.politico.com"
  ];
}
