{ config, pkgs, ... }:
{
  environment.etc."profiles/joshua.png" = {
    source = ./joshua.png;  # Relative to your config file
    mode = "0644";
  };

  environment.etc."AccountsService/users/joshua" = {
    text = ''
      [User]
      SystemAccount=false
      Icon=/etc/profiles/joshua.png
    '';
    mode = "0644";
  };

  services.accounts-daemon.enable = true;
}
