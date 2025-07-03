{ config, pkgs, ... }:

{
  # Just symlink your configs for live editing
  home.file.".config/hypr".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/hypr";

  home.file.".config/waybar".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/waybar";

  home.file.".config/swaync".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/nixos-config/dotfiles/swaync";

  home.file.".config/wofi".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nixos-config/dotfiles/wofi";
}
