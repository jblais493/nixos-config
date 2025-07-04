{ config, pkgs, ... }:
{
  imports = [
    ./doom.nix
  ];

  # CLI/TUI tools
  environment.systemPackages = with pkgs; [
    # Editor
    neovim
  ];
}
