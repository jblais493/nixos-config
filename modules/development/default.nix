{ config, pkgs, ... }:
{
  imports = [
    ./base.nix
    ./doom.nix
    ./go.nix
    ./python.nix
    ./rust.nix
  ];
}
