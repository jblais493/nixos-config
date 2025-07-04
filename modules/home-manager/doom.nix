{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      # Essential packages that Doom might need
      emacsPackages.vterm
      emacsPackages.pdf-tools
      emacsPackages.org-roam
    ];
  };

  # Install Doom Emacs
  home.activation.installDoomEmacs = config.lib.dag.entryAfter ["writeBoundary"] ''
    if [ ! -d "${config.home.homeDirectory}/.emacs.d" ]; then
      echo "Installing Doom Emacs..."
      ${pkgs.git}/bin/git clone --depth 1 https://github.com/doomemacs/doomemacs ${config.home.homeDirectory}/.emacs.d

      # Set up environment for Doom installation
      export PATH="${pkgs.emacs}/bin:${pkgs.git}/bin:${pkgs.ripgrep}/bin:${pkgs.fd}/bin:$PATH"

      # Install Doom (this will use your symlinked config)
      ${config.home.homeDirectory}/.emacs.d/bin/doom install --no-env --no-hooks
    fi
  '';

  # Sync Doom configuration when dotfiles change
  home.activation.syncDoomEmacs = config.lib.dag.entryAfter ["linkGeneration"] ''
    if [ -d "${config.home.homeDirectory}/.emacs.d" ] && [ -d "${config.home.homeDirectory}/.config/doom" ]; then
      echo "Syncing Doom configuration..."
      export PATH="${pkgs.emacs}/bin:${pkgs.git}/bin:${pkgs.ripgrep}/bin:${pkgs.fd}/bin:$PATH"
      ${config.home.homeDirectory}/.emacs.d/bin/doom sync
    fi
  '';

  # Add Doom to PATH
  home.sessionPath = [ "${config.home.homeDirectory}/.emacs.d/bin" ];

  # Environment variables for Doom
  home.sessionVariables = {
    DOOMDIR = "${config.home.homeDirectory}/.config/doom";
    DOOMLOCALDIR = "${config.home.homeDirectory}/.local/share/doom";
  };
}
