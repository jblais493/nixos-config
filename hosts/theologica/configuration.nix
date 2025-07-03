{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/shared
    ../../modules/desktop      # This imports the desktop/default.nix
    ../../modules/cli-tui
    ../../modules/development
    ../../modules/media
    # ... other imports
  ];

  # Host-specific configuration
  networking.hostName = "theologica";
  # ... rest of your host config
}
