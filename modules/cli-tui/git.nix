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
    };
  };

  # Install git in system packages too
  environment.systemPackages = with pkgs; [
    git
  ];
}
