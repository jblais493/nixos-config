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

  # Enable Emacs daemon service
  services.emacs = {
    enable = true;
    package = config.programs.emacs.finalPackage; # Use the same package as programs.emacs
    defaultEditor = true;

    # Start daemon with graphical session (Hyprland)
    startWithUserSession = "graphical";

    # Enable socket activation (starts daemon on-demand)
    socketActivation.enable = true;

    # Additional daemon arguments
    client = {
      enable = true;
      arguments = [
        "-c"
        "-a"
        "''"
      ]; # Default to GUI frames
    };
  };

  # Doom paths
  home.sessionPath = [
    "${config.home.homeDirectory}/.config/emacs/bin"
  ];

  home.sessionVariables = {
    DOOMDIR = "${config.home.homeDirectory}/.config/doom";
    DOOMLOCALDIR = "${config.home.homeDirectory}/.config/emacs/.local";
    EDITOR = "emacsclient -t";
    VISUAL = "emacsclient -c -a ''";
  };

  # Shell aliases for convenience
  home.shellAliases = {
    e = "emacsclient -t -a ''"; # Terminal Emacs
    ec = "emacsclient -c -a ''"; # GUI Emacs
    ekill = "emacsclient -e '(kill-emacs)'"; # Kill daemon
    erestart = "emacsclient -e '(kill-emacs)' && emacs --daemon"; # Restart daemon
  };
}
