{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    age
    keychain

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
