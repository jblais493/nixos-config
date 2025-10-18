{ config, pkgs, ... }:
{
  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    pinentryPackage = pkgs.pinentry-gtk2; # Note: newer syntax
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };
}
