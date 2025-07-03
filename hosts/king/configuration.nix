{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/desktop      # This imports the desktop/default.nix
    ../../modules/cli-tui
    ../../modules/development
    ../../modules/media
    ../../modules/home-manager
    ../../modules/security
    # ... other imports
  ];

  # Host-specific configuration
  networking.hostName = "king";
  # ... rest of your host config
}
