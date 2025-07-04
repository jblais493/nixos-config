{ config, pkgs, ... }:
{
  # Install Emacs and Doom dependencies at system level
  environment.systemPackages = with pkgs; [
    emacs
    # Doom Emacs dependencies
    ripgrep
    fd
    imagemagick
    texlive.combined.scheme-full  # For LaTeX support
    sqlite       # For org-roam

    # Additional tools Doom might need
    git
    curl
    wget
  ];
}
