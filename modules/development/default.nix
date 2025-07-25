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
    glibc
    clang
    cmake
    libtool
    gnumake
    sdbus-cpp
    nixd  # Nix LSP
    direnv
    keychain

    # Rust related
   rustc
   cargo
   rustfmt
   rust-analyzer
   clippy
  ];
}
