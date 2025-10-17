{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    age

    # Password management
    (pass-wayland.withExtensions (
      exts: with exts; [
        pass-otp
        pass-import
        pass-audit
      ]
    ))
  ];
}
