{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    mu
    isync
    msmtp

    gnutls
  ];
}
