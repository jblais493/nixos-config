{ config, pkgs, ... }:

{
  # Install Emacs
  home.packages = with pkgs; [
    emacs
    # Doom Emacs dependencies
    git
    ripgrep
    fd
    imagemagick
    texliveFull  # For LaTeX support
    sqlite       # For org-roam
  ];

  # Symlink your entire Doom Emacs configuration
  home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/doom";

  # Also symlink the +STORE directory if Doom uses it
  home.file.".config/+STORE".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/+STORE";
}
