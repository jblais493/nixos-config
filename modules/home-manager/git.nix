{ config, pkgs, ... }:
{
  programs.git = {
    enable = true;
    userName = "Joshua Blais";
    userEmail = "josh@joshblais.com";

    extraConfig = {
      init.defaultBranch = "main";
      core = {
        editor = "nvim";
        autocrlf = "input";
      };
      pull.rebase = true;
      push.autoSetupRemote = true;
    };
  };
}
