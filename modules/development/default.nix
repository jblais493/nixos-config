{ config, pkgs, ... }:
{
  imports = [
    ./doom.nix
  ];

  # CLI/TUI tools
  environment.systemPackages = with pkgs; [
    # Editor
    neovim
    hugo
    go
    python314
    gcc
    clang
    cmake
    libtool
    gnumake
    nixd  # Nix LSP
    direnv
  ];
}
