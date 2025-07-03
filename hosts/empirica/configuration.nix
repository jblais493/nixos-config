# { config, pkgs, inputs, ... }:
# {
#   imports = [
#     ./hardware-configuration.nix
#     ../../modules/cli-tui
#     ../../modules/security
#     ../../modules/services
#     # ... other imports
#   ];

#   # Host-specific configuration
#   networking.hostName = "empirica";
#   # ... rest of your host config
# }
