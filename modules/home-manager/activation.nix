{ config, pkgs, ... }:

let
  # Create a complete environment for Doom operations
  doomEnv = pkgs.buildEnv {
    name = "doom-env";
    paths = with pkgs; [
      emacs
      git
      ripgrep
      fd
      coreutils
      findutils
      gnutar
      gzip
      gnused
      gnugrep
      bash
    ];
  };
in
{
  home.activation = {
    # Repository cloning
    cloneRepos = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      if ${pkgs.openssh}/bin/ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
        if [ ! -d "${config.home.homeDirectory}/.config/scripts" ]; then
          echo "Cloning private scripts repo..."
          ${pkgs.git}/bin/git clone git@github.com:jblais493/scripts.git \
            ${config.home.homeDirectory}/.config/scripts
        fi
      else
        echo "SSH key not configured for GitHub, skipping private scripts repo"
      fi

      if [ ! -d "${config.home.homeDirectory}/Pictures/Wallpapers" ]; then
        ${pkgs.git}/bin/git clone https://github.com/jblais493/Wallpapers \
          ${config.home.homeDirectory}/Pictures/Wallpapers
      fi

      if [ ! -d "${config.home.homeDirectory}/.config/kmonad" ]; then
        ${pkgs.git}/bin/git clone https://github.com/jblais493/Kmonad-thinkpad \
          ${config.home.homeDirectory}/.config/kmonad
      fi
    '';

    # Doom Emacs installation
    installDoomEmacs = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -d "${config.home.homeDirectory}/.emacs.d" ]; then
        echo "Installing Doom Emacs..."
        # Completely bypass user git config by unsetting HOME temporarily
        HOME=/tmp ${pkgs.git}/bin/git clone --depth 1 \
          https://github.com/doomemacs/doomemacs \
          ${config.home.homeDirectory}/.emacs.d

        if [ -x "${config.home.homeDirectory}/.emacs.d/bin/doom" ]; then
          echo "Running doom install..."
          export PATH="${doomEnv}/bin:$PATH"
          ${config.home.homeDirectory}/.emacs.d/bin/doom install --no-env --no-hooks
        else
          echo "Error: doom binary not found after clone"
          exit 1
        fi
      else
        echo "Doom Emacs already installed"
      fi
    '';

    # Doom sync
    syncDoomEmacs = config.lib.dag.entryAfter [ "linkGeneration" "installDoomEmacs" ] ''
      if [ -d "${config.home.homeDirectory}/.emacs.d" ] && \
         [ -d "${config.home.homeDirectory}/.config/doom" ]; then
        if [ -x "${config.home.homeDirectory}/.emacs.d/bin/doom" ]; then
          echo "Syncing Doom configuration..."
          export PATH="${doomEnv}/bin:$PATH"
          ${config.home.homeDirectory}/.emacs.d/bin/doom sync
        else
          echo "Warning: doom binary not found, skipping sync"
        fi
      else
        echo "Doom or doom config not found, skipping sync"
      fi
    '';
  };
}
