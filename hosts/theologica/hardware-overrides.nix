# Hardware-specific overrides that don't belong in auto-generated hardware-configuration.nix
{ config, pkgs, lib, ... }:
{
  # AMD Radeon 680M (Rembrandt) Graphics Configuration
  hardware.graphics = {
    enable = true;
    enable32Bit = true;  # Replaces driSupport32Bit

    extraPackages = with pkgs; [
      amdvlk              # AMD Vulkan driver
      libva               # Video acceleration API
      vaapiVdpau          # VDPAU backend for VAAPI
      libvdpau-va-gl      # VDPAU driver
      rocminfo            # ROCm system info
      rocm-opencl-icd     # OpenCL support
    ];

    extraPackages32 = with pkgs.pkgsi686Linux; [
      amdvlk              # 32-bit Vulkan for compatibility
    ];
  };

  # AMD-specific environment variables
  environment.variables = {
    AMD_VULKAN_ICD = "RADV";           # Use open-source RADV driver
    LIBVA_DRIVER_NAME = "radeonsi";    # Video acceleration driver
  };
}
