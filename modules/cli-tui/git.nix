{ config, pkgs, ... }:
{
  programs.git = {
    enable = true;
    config = {
      init.defaultBranch = "main";
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

      # URL rewriting - force SSH for all forges
      url."git@github.com:".insteadOf = "https://github.com/";
      url."git@codeberg.org:".insteadOf = "https://codeberg.org/";
      url."git@forge.labrynth.org:".insteadOf = "https://forge.labrynth.org/";
    };
  };

  # Aliases must go in extraConfig for system-level git
  programs.git.config = {
    alias = {
      # Multi-forge push
      mf-push = "!git remote | grep -E '(origin|github|codeberg|forgejo)' | xargs -I {} git push {} $(git rev-parse --abbrev-ref HEAD)";

      # Setup all remotes
      mf-init = "!f() { REPO=$1; git remote add github git@github.com:jblais493/$REPO.git; git remote add codeberg git@codeberg.org:joshuablais/$REPO.git; git remote add forgejo git@forge.labrynth.org:josh/$REPO.git; echo 'Multi-forge remotes configured'; git remote -v; }; f";

      # Individual remote additions
      add-github = "!f() { git remote add github git@github.com:jblais493/$1.git; }; f";
      add-codeberg = "!f() { git remote add codeberg git@codeberg.org:joshuablais/$1.git; }; f";
      add-forgejo = "!f() { git remote add forgejo git@forge.labrynth.org:josh/$1.git; }; f";

      # Convenience
      st = "status -sb";
      co = "checkout";
      br = "branch";
      last = "log -1 HEAD";
      unstage = "reset HEAD --";
    };
  };

  environment.systemPackages = with pkgs; [
    git
    lazygit
    tea
  ];
}
