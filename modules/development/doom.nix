{ config, pkgs, ... }:

{
  # Install Emacs
  home.packages = with pkgs; [
    emacs
    # Doom Emacs dependencies
    git
    ripgrep
    fd
    imagemagick
    texliveFull  # For LaTeX support
    sqlite       # For org-roam
  ];
