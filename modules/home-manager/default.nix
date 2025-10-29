{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    inputs.stylix.homeManagerModules.stylix
    ./activation.nix
    ./dotfiles.nix
    ./emacs.nix
    ./firefox.nix
    ./gpg.nix
    ./media.nix
    ./theming.nix
    ./xdg.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
  };
}
