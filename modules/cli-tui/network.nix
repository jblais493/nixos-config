{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget
    curl
    httrack
    nmap
    dig
  ];
}
