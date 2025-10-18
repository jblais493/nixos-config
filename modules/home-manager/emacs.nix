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

  services.emacs = {
    enable = true;
    package = config.programs.emacs.finalPackage;
    defaultEditor = true; # This handles EDITOR/VISUAL automatically
    startWithUserSession = "graphical";
    socketActivation.enable = true;
    client = {
      enable = true;
      arguments = [
        "-c"
        "-a"
        "''"
      ];
    };
  };

  # Doom paths
  home.sessionPath = [
    "${config.home.homeDirectory}/.config/emacs/bin"
  ];

  home.sessionVariables = {
    DOOMDIR = "${config.home.homeDirectory}/.config/doom";
    DOOMLOCALDIR = "${config.home.homeDirectory}/.config/emacs/.local";
    # EDITOR and VISUAL removed—handled by services.emacs.defaultEditor
  };

  home.shellAliases = {
    e = "emacsclient -t -a ''";
    ec = "emacsclient -c -a ''";
    ekill = "emacsclient -e '(kill-emacs)'";
    erestart = "emacsclient -e '(kill-emacs)' && emacs --daemon";
  };
}
