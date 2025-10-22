{
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./networking.nix
  ];

  # Fix NUR overlay reference
  nixpkgs.overlays = [ inputs.nur.overlays.default ];

  # Enable flakes and trusted users
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [
      "root"
      "joshua"
    ]; # Add this line
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
