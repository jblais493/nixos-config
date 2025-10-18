{ lib, ... }:
{
  nix.settings = {
    # Core features
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [
      "root"
      "joshua"
    ];

    # Store optimization
    auto-optimise-store = true;
    max-jobs = "auto";

    # Build optimization
    cores = 0;
    sandbox = true;

    # Substituters for faster downloads
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    # Additional performance tweaks
    keep-outputs = true; # Keep build outputs for debugging
    keep-derivations = true; # Keep .drv files
    warn-dirty = false; # Don't warn about dirty git trees (workstation convenience)
  };

  # Default GC - can be overridden by profiles
  nix.gc = lib.mkDefault {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Optimize store on schedule
  nix.optimise = {
    automatic = true;
    dates = [ "03:45" ];
  };

  nixpkgs.config.allowUnfree = true;

  # Clean up old boot entries automatically
  boot.loader.systemd-boot.configurationLimit = lib.mkDefault 10;
}
