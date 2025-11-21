{ lib, ... }:
{
  imports = [
    ../modules/cli-tui
    ../modules/development
    ../modules/desktop
    ../modules/media
    ../modules/security
    ../modules/shared
  ];

  time.timeZone = lib.mkDefault "America/Edmonton";
  i18n.defaultLocale = "en_CA.UTF-8";
  boot.loader.systemd-boot.configurationLimit = 20;

  # Optimizations
  nix = {
    # Auto-optimize store daily (deduplicates files)
    settings.auto-optimise-store = true;

    # Auto garbage-collect weekly
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d"; # Keep last 2 weeks of builds
    };
  };

  system.activationScripts.userAvatar = ''
      mkdir -p /var/lib/AccountsService/{icons,users}
      cp ${./assets/joshua.png} /var/lib/AccountsService/icons/joshua
      chmod 644 /var/lib/AccountsService/icons/joshua
      cat > /var/lib/AccountsService/users/joshua << 'EOF'
    [User]
    Icon=/var/lib/AccountsService/icons/joshua
    EOF
  '';
}
