{ config, pkgs, ... }:
{
  # Enable CUPS for printing
  services.printing.enable = true;
}
