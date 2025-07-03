{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # PDF viewers
    zathura
    evince
    kdePackages.okular

    # PDF tools
    poppler_utils
    pdftk

    # Document conversion
    pandoc
    texlive.combined.scheme-full
  ];
}
