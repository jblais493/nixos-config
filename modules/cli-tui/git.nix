{ config, pkgs, ... }:
{
  programs.git = {
    enable = true;
    config = {
      init.defaultBranch = "master";
      user = {
        name = "Joshua Blais";
        email = "josh@joshblais.com";
      };
      core = {
        editor = "nvim";
        autocrlf = "input";
      };
      pull.rebase = true;
      push.autoSetupRemote = true;

      rebase = {
        autoSquash = true;
        autoStash = true;
      };

      fetch.prune = true;

      diff = {
        algorithm = "histogram";
        colorMoved = "default";
      };

      merge.conflictStyle = "zdiff3";

      # GitHub configuration
      github.user = "jblais493";

      # URL rewriting - force SSH for GitHub (no HTTPS auth needed)
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };

  environment.systemPackages = with pkgs; [
    git
    lazygit
    tea
  ];
}
