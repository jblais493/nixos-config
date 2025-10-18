{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ./setup.nix
    ./theming.nix
    ./firefox.nix
    ./gpg.nix
    ./core/default.nix
    ./core/xdg.nix
    ./development/emacs.nix
    ./system/dotfiles.nix
  ];

  home.username = "joshua";
  home.homeDirectory = "/home/joshua";
  home.stateVersion = "25.05";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "/home/joshua/MusicOrganized";
  };
}
