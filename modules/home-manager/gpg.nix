{ config, pkgs, ... }:
{
  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    maxCacheTtl = 86400;
    pinentry.package = pkgs.pinentry-gtk2; # Fixed: pinentryPackage → pinentry.package
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = {
        addKeysToAgent = "yes"; # Fixed: moved into matchBlocks
        identityFile = [
          "~/.ssh/empire.key"
          "~/.ssh/id_ed25519"
        ];
      };
    };
  };
}
