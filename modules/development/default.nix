{ config, pkgs, ... }:
{
  imports = [
      ./base.nix
      ./doom.nix
      ./go.nix
      ./rust.nix
      ./python.nix
  ];
}
