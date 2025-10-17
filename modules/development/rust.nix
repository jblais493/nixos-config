{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    rustc
    cargo
    rustfmt
    rust-analyzer
    clippy
  ];
}
