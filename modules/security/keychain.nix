{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    keychain
  ];
}
