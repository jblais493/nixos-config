{ config, pkgs, ... }:

{
  imports = [
    ./tmux.nix
    ./zsh.nix
  ];
}
