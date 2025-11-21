{ config, pkgs, ... }:
{
  # System-level browser installations
  environment.systemPackages = with pkgs; [
    librewolf
    ungoogled-chromium
    tor-browser
  ];
}
