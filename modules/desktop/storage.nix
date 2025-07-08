{ config, pkgs, ... }:
{
 # Enable automatic mounting of USB drives
  services.udisks2.enable = true;

  # Enable GVFS for desktop integration
  services.gvfs.enable = true;

  # Force start udisks2
  systemd.services.udisks2 = {
    wantedBy = [ "graphical-session.target" ];
  };

  # Enable polkit for user permissions
  security.polkit.enable = true;

  # Use GNOME's polkit agent
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  # Additional packages for USB/storage management
  environment.systemPackages = with pkgs; [
    # File manager integration
    gvfs          # GNOME Virtual File System
    udisks2       # Disk management service

    # Disk management tools
    gnome-disk-utility  # Disk utility GUI with image writing
    gparted       # Partition editor

    # Image writing tools
    isoimagewriter   # Simple USB image writer
    # or alternatively:
    # gnome-multi-writer  # GNOME's USB writer
    # raspberry-pi-imager # Official Pi imager (works for other images too)

    # Command line tools
    dd_rescue     # Better dd with error recovery
    pv            # Progress viewer for dd operations
  ];
}
