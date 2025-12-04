{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Enable NVIDIA drivers
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      libvdpau-va-gl
      nvidia-vaapi-driver
    ];
  };

  hardware.nvidia = {
    # Use latest production driver
    package = config.boot.kernelPackages.nvidiaPackages.stable;

    # Modesetting required for Wayland
    modesetting.enable = true;

    # Power management (critical for stability)
    powerManagement.enable = true;
    powerManagement.finegrained = false;

    # Open source kernel module - try true first for RTX 3080
    open = false;

    # Enable nvidia-settings
    nvidiaSettings = true;
  };

  # Prevent random freezes - critical for RTX 3080
  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
  ];

  # Kernel modules
  boot.extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
  boot.kernelModules = [
    "nvidia"
    "nvidia_modeset"
    "nvidia_uvm"
    "nvidia_drm"
  ];

  # Wayland environment variables for Hyprland + NVIDIA
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "nvidia";
    XDG_SESSION_TYPE = "wayland";
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    WLR_NO_HARDWARE_CURSORS = "1"; # Fixes cursor issues on NVIDIA
    NIXOS_OZONE_WL = "1"; # Enable Wayland for Electron apps
  };
}
