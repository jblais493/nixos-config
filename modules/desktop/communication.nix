{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    signal-desktop
    telegram-desktop
    dino
    thunderbird
  ];
}
