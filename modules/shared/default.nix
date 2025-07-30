{ config, pkgs, inputs, ... }:
{
  imports = [
    ./networking.nix
  ];

  # Fix NUR overlay reference
  nixpkgs.overlays = [ inputs.nur.overlays.default ];

  # Common configuration for all hosts
  time.timeZone = "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";

 # Enable flakes and trusted users
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "root" "joshua" ];  # Add this line
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
