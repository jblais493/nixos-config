{ config, pkgs, ... }:

{
  imports = [
    ./core-utils.nix
    ./nix.nix
    ./git.nix
    ./system-tools.nix
    ./security.nix
    ./network.nix
    ./neovim.nix
    ./podman.nix
    ./shell
  ];
}
