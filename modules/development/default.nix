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
    # Go toolchain
    go
    gopls
    godef
    gotools

    (python313.withPackages (ps: with ps; [
      tweepy
      mastodon-py
    ]))
    gcc
    gh
    glibc
    clang
    cmake
    libtool
    gnumake
    sdbus-cpp
    keychain
    pciutils

    # Rust related
    rustc
    cargo
    rustfmt
    rust-analyzer
    clippy
  ];
}
