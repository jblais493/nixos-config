{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    # Nix workflow
    nixd # Nix LSP
    nvd # Nix/NixOS package version diff tool
    nh # nix helper
    nixfmt # nix formatting
    deploy-rs # deployment
    direnv # start environments on cd into directory
    devenv # development environments
    nix-output-monitor # nix output monitoring
    deadnix # Make sure code is getting used
    comma # run packages without installing
  ];

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
}
