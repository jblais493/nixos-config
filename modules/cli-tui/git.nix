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
    };
  };

  environment.systemPackages = with pkgs; [
    git
    lazygit # Git TUI
    tea # Gitea CLI (if you use Gitea)
  ];
}
