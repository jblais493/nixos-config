{ config, lib, pkgs, ... }:

{
  # Core power management services
  services = {
    # Explicitly disabled since we are using TLP
    power-profiles-daemon.enable = false;

    # upower: The D-Bus service providing power enumeration and control
    # This is your foundation - everything else builds upon it
    upower = {
      enable = true;
      percentageLow = 15;
      percentageCritical = 5;
      percentageAction = 3;
      criticalPowerAction = "Hibernate";
    };

    # TLP: The sophisticated power management daemon
    # This is your primary weapon - comprehensive and battle-tested
    tlp = {
      enable = true;
      settings = {
        # CPU scaling governor - ondemand balances performance/efficiency
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        # AMD-specific: Your Ryzen 6000 series benefits from these
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

        # Platform profile for modern AMD systems
        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "low-power";

        # PCIe power management
        PCIE_ASPM_ON_AC = "default";
        PCIE_ASPM_ON_BAT = "powersupersave";

        # WiFi power saving
        WIFI_PWR_ON_AC = "off";
        WIFI_PWR_ON_BAT = "on";

        # USB autosuspend - aggressive but effective
        USB_AUTOSUSPEND = 1;

        # Disk parameters
        DISK_DEVICES = "nvme0n1";
        DISK_APM_LEVEL_ON_AC = "254";
        DISK_APM_LEVEL_ON_BAT = "128";
      };
    };

    # Thermald: Thermal management daemon
    # Critical for your T14s - prevents thermal throttling
    thermald.enable = true;

    # Power profiles daemon (alternative approach)
    # Uncomment if you prefer this over TLP - don't run both
    # power-profiles-daemon.enable = true;
  };

  # Essential power management packages
  environment.systemPackages = with pkgs; [
    # Analysis and monitoring tools
    powertop          # Your power consumption analyzer
    acpi              # ACPI information utility
    lm_sensors        # Hardware monitoring

    # Advanced tools for the power user
    cpupower-gui      # CPU frequency scaling GUI
    auto-cpufreq      # Automatic CPU frequency scaling

    # Battery health monitoring
    acpi    # For advanced battery calibration
  ];

  # Kernel parameters for optimal power management
  boot.kernelParams = [
    # AMD-specific optimizations for your Ryzen 6000
    "amd_pstate=active"

    # Memory power management
    "mem_sleep_default=deep"

    # PCIe power saving
    "pcie_aspm.policy=powersupersave"
  ];

  # Kernel modules for advanced power features
  boot.kernelModules = [ "msr" "cpuid" ];

  # Enable firmware updates for power efficiency improvements
  services.fwupd.enable = true;

  # Laptop-specific power optimizations
  powerManagement = {
    enable = true;
    powertop.enable = true;  # Enables powertop auto-tuning service
    cpuFreqGovernor = lib.mkDefault "ondemand";
  };

  # Hardware-specific optimizations for T14s Gen 3
  hardware = {
    # Enable CPU microcode updates
    cpu.amd.updateMicrocode = true;
  };

  # Suspend/hibernate configuration
  systemd = {
    sleep.extraConfig = ''
      HibernateDelaySec=1800
      SuspendState=mem
    '';

    # Power-aware systemd services
    services = {
      # Disable unnecessary services on battery
      "NetworkManager-wait-online".enable = false;

      # Custom power optimization service
      power-optimization = {
        description = "Custom power optimizations";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          # Enable laptop mode
          echo 5 > /proc/sys/vm/laptop_mode

          # Optimize writeback parameters
          echo 1500 > /proc/sys/vm/dirty_writeback_centisecs
          echo 6000 > /proc/sys/vm/dirty_expire_centisecs
        '';
      };
    };
  };
}
