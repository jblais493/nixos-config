{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Use DaVinci Resolve (already from unstable since your main input is unstable)
  environment.systemPackages = [
    pkgs.davinci-resolve
  ];

  # Increase file watchers - Resolve uses massive amounts
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 524288;
    "fs.inotify.max_user_instances" = 512;
    "fs.file-max" = 2097152;
  };

  # System resource limits for Resolve
  security.pam.loginLimits = [
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "65536";
    }
    {
      domain = "*";
      type = "hard";
      item = "nofile";
      value = "131072";
    }
    {
      domain = "*";
      type = "soft";
      item = "memlock";
      value = "unlimited";
    }
    {
      domain = "*";
      type = "hard";
      item = "memlock";
      value = "unlimited";
    }
  ];

  # Force X11 for Resolve - doesn't work properly on Wayland
  environment.sessionVariables = {
    RESOLVE_FORCE_X11 = "1";
  };

  # User needs video/audio/render groups for hardware access
  users.users.joshua = {
    extraGroups = [
      "video"
      "audio"
      "render"
    ];
  };

  # Optional: Create cache directory with proper permissions
  systemd.tmpfiles.rules = [
    "d /var/cache/resolve 0755 joshua users -"
  ];
}
