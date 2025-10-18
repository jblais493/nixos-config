{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.stateVersion = "25.05";
  programs.home-manager.enable = true;
  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
  };
}
