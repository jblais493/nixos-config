{ config, pkgs, inputs, ... }:
{
  imports = [
    ./networking.nix
  ];

  # Add NUR overlay for all hosts that import shared
  nixpkgs.overlays = [ inputs.nur.overlay ];

  # Common configuration for all hosts
  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
