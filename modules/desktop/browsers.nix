{ config, pkgs, ... }:
{
  # System-level browser installations
  environment.systemPackages = with pkgs; [
    brave
    tor-browser
  ];

  # Firefox with extensions via home-manager approach
programs.firefox = {
    enable = true;
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      BlockAboutConfig = false;
      OfferToSaveLogins = false;
    };
  };
}
