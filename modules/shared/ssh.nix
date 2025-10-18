{ lib, ... }:
{
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
    };

    # Allow SSH agent forwarding for your user
    extraConfig = ''
      Match User joshua
        AllowAgentForwarding yes
    '';
  };

  # Fail2ban for SSH hardening
  services.fail2ban = {
    enable = lib.mkDefault true;
    maxretry = 3;
    bantime = "1h";
  };
}
