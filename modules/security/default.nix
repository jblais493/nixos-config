{ config, pkgs, ... }:
{
  imports = [
    ./keychain.nix
    # Add other security modules here as you create them
  ];

  # Basic security hardening
  security = {
    sudo.wheelNeedsPassword = true;

    # Disable root login
    sudo.enable = true;
  };

  # SSH hardening
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      Protocol = 2;
    };
  };

  # Firewall
  networking.firewall.enable = true;

  # Security packages
  environment.systemPackages = with pkgs; [
    fail2ban
    ufw
    gnupg
    age
  ];
}
