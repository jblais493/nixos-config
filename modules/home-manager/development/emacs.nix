{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages =
      epkgs: with epkgs; [
        doom
        vterm
        pdf-tools
        org-roam
        treesit-grammars.with-all-grammars
      ];
  };

  home.sessionPath = [ "${config.home.homeDirectory}/.emacs.d/bin" ];

  home.sessionVariables = {
    DOOMDIR = "${config.home.homeDirectory}/.config/doom";
    DOOMLOCALDIR = "${config.home.homeDirectory}/.local/share/doom";
  };
}
