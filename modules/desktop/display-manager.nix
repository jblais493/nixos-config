{ config, pkgs, ... }:
{
  # Copy your profile image to the system store
  environment.etc."profiles/joshua.png" = {
    source = /home/joshua/Pictures/joshua.png;
    mode = "0644";
  };

  # Configure AccountsService for your user
  environment.etc."AccountsService/users/joshua" = {
    text = ''
      [User]
      SystemAccount=false
      Icon=/etc/profiles/joshua.png
    '';
    mode = "0644";
  };

  # Ensure AccountsService is enabled (usually automatic with GDM)
  services.accounts-daemon.enable = true;
}
