{ config, ... }:

let
  mkOutOfStoreSymlink = path: config.lib.file.mkOutOfStoreSymlink path;
  configDir = "${config.home.homeDirectory}/nixos-config/dotfiles";
in
{
  home.file = {
    ".config/bat".source = mkOutOfStoreSymlink "${configDir}/bat";
    ".config/btop".source = mkOutOfStoreSymlink "${configDir}/btop";
    ".config/doom".source = mkOutOfStoreSymlink "${configDir}/doom";
    ".config/fastfetch".source = mkOutOfStoreSymlink "${configDir}/fastfetch";
    ".config/gowall".source = mkOutOfStoreSymlink "${configDir}/gowall";
    ".config/hypr".source = mkOutOfStoreSymlink "${configDir}/hypr";
    ".config/kitty".source = mkOutOfStoreSymlink "${configDir}/kitty";
    ".config/mpd".source = mkOutOfStoreSymlink "${configDir}/mpd";
    ".config/mpv".source = mkOutOfStoreSymlink "${configDir}/mpv";
    ".config/nvim".source = mkOutOfStoreSymlink "${configDir}/nvim";
    ".config/starship.toml".source = mkOutOfStoreSymlink "${configDir}/starship/starship.toml";
    ".config/swaync".source = mkOutOfStoreSymlink "${configDir}/swaync";
    ".config/tmux/plugins".source = mkOutOfStoreSymlink "${configDir}/tmux/plugins";
    ".config/waybar".source = mkOutOfStoreSymlink "${configDir}/waybar";
    ".config/wofi".source = mkOutOfStoreSymlink "${configDir}/wofi";
    ".config/yt-dlp".source = mkOutOfStoreSymlink "${configDir}/yt-dlp";
    ".config/zathura".source = mkOutOfStoreSymlink "${configDir}/zathura";
    ".tmux.conf".source = mkOutOfStoreSymlink "${configDir}/tmux/.tmux.conf";
    ".tridactylrc".source = mkOutOfStoreSymlink "${configDir}/tridactyl/tridactylrc";
    ".zshrc".source = mkOutOfStoreSymlink "${configDir}/zsh/.zshrc";
    # Thunderbird theming
    ".thunderbird/w2wlpdok.default/chrome".source =
      mkOutOfStoreSymlink "${configDir}/thunderbird/chrome";
  };
}
