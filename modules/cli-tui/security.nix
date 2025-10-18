{ config, pkgs, ... }:
{
  # Install security tools system-wide
  environment.systemPackages = with pkgs; [
    gnupg
    age
    pinentry-gtk2 # Add this so it's available
    (pass-wayland.withExtensions (
      exts: with exts; [
        pass-otp
        pass-import
        pass-audit
      ]
    ))
  ];
}
