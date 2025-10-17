{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    go
    gopls
    godef
    gotools
  ];
}
