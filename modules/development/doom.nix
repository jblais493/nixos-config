{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    ripgrep
    vips
    wmctrl
    fd
    imagemagick
    texlive.combined.scheme-full
    sqlite
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    git
    curl
    wget
    nodejs
    nodePackages.js-beautify
    shellcheck
    html-tidy
    stylelint
    ledger
    shfmt
    graphviz
    emacs-all-the-icons-fonts
  ];
}
