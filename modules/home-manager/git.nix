{ config, pkgs, ... }:
{
  programs.git = {
    enable = true;
    userName = "Joshua Blais";
    userEmail = "josh@joshblais.com";

    extraConfig = {
      credential.helper = "store";
      init.defaultBranch = "master";
      core = {
        editor = "nvim";
        autocrlf = "input";
      };
      pull.rebase = true;
      push.autoSetupRemote = true;

      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };
}
