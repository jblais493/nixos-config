{ config, pkgs, ... }:
{
  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    maxCacheTtl = 86400;
    pinentryPackage = pkgs.pinentry-gtk2;
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      "*" = {
        identityFile = [
          "~/.ssh/empire.key"
          "~/.ssh/id_ed25519"
        ];
      };
    };
  };

  # Ensure SSH_AUTH_SOCK is set in shell
  programs.zsh = {
    initExtra = ''
      export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
    '';
  };
}
