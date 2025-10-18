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

  programs.zsh = {
    initExtra = ''
      # Set SSH_AUTH_SOCK for gpg-agent
      export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"

      # Auto-load SSH keys once per session
      if ! ssh-add -l 2>/dev/null | grep -q "empire beginning ssh key"; then
        ssh-add ~/.ssh/empire.key
      fi
      if ! ssh-add -l 2>/dev/null | grep -q "joshua@joshuablais.com"; then
        ssh-add ~/.ssh/id_ed25519
      fi
    '';
  };
}
