{ config, pkgs, ... }:

{
  # Install tmux
  home.packages = with pkgs; [ tmux ];

  # Create symlink to your dotfile using mkOutOfStoreSymlink
  home.file.".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/tmux.conf";

  # Also symlink your tmux plugins directory if needed
  home.file.".config/tmux".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/tmux";
}
