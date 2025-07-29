{ config, pkgs, ... }:
{
  # Clone repositories - skip private repos if SSH key not available
  home.activation.cloneRepos = config.lib.dag.entryAfter ["writeBoundary"] ''
    # Check if SSH key exists and GitHub is accessible
    if ${pkgs.openssh}/bin/ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
      # Scripts repo (private - only if SSH works)
      if [ ! -d "${config.home.homeDirectory}/.config/scripts" ]; then
        echo "Cloning private scripts repo..."
        ${pkgs.git}/bin/git clone git@github.com:jblais493/scripts.git ${config.home.homeDirectory}/.config/scripts
      fi
    else
      echo "SSH key not configured for GitHub, skipping private scripts repo"
    fi

    # Public repos
    if [ ! -d "${config.home.homeDirectory}/Pictures/Wallpapers" ]; then
      ${pkgs.git}/bin/git clone https://github.com/jblais493/Wallpapers ${config.home.homeDirectory}/Pictures/Wallpapers
    fi

    if [ ! -d "${config.home.homeDirectory}/.config/kmonad" ]; then
      ${pkgs.git}/bin/git clone https://github.com/jblais493/Kmonad-thinkpad ${config.home.homeDirectory}/.config/kmonad
    fi
  '';

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-gtk2;
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };
}
