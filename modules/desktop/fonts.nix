{ config, pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      alegreya
      # New nerd-fonts namespace
      nerd-fonts.geist-mono
      montserrat
      bitter
      # Add any other fonts you need
    ];

    # Font configuration
    fontconfig = {
      enable = true;
      defaultFonts = {
        serif = [ "Alegreya" ];
        sansSerif = [ "Montserrat" ];
        monospace = [ "GeistMono Nerd Font" ];
      };
    };
  };
}
