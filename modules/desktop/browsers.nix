{ config, pkgs, ... }:
{
  # System-level browser installations
  environment.systemPackages = with pkgs; [
    brave
  ];

  # Firefox with extensions via home-manager approach
  programs.firefox = {
    enable = true;

    # System-wide policies (optional)
    policies = {
      # Disable telemetry
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;

      # Security settings
      BlockAboutConfig = false;
      OfferToSaveLogins = false;
    };
  };
}
