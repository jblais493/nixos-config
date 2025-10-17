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
    nh # nix helper
    nixfmt # nix formatting
    deploy-rs # deployment
    direnv # start environments on cd into directory
    devenv # development environments
    nix-output-monitor # nix output monitoring
    deadnix # Make sure code is getting used
    comma # run packages without installing
  ];
}
