{ config, ... }:

let
  mkOutOfStoreSymlink = path: config.lib.file.mkOutOfStoreSymlink path;
  configDir = "${config.home.homeDirectory}/nixos-config/dotfiles";
in
{
  home.file = {
    ".config/doom".source = mkOutOfStoreSymlink "${configDir}/doom";
    ".zshrc".source = mkOutOfStoreSymlink "${configDir}/zsh/.zshrc";
    ".config/starship.toml".source = mkOutOfStoreSymlink "${configDir}/starship/starship.toml";
    ".config/tmux/plugins".source = mkOutOfStoreSymlink "${configDir}/tmux/plugins";
    ".config/hypr".source = mkOutOfStoreSymlink "${configDir}/hypr";
    ".config/kitty".source = mkOutOfStoreSymlink "${configDir}/kitty";
    ".config/waybar".source = mkOutOfStoreSymlink "${configDir}/waybar";
    ".config/swaync".source = mkOutOfStoreSymlink "${configDir}/swaync";
    ".config/wofi".source = mkOutOfStoreSymlink "${configDir}/wofi";
    ".config/nvim".source = mkOutOfStoreSymlink "${configDir}/nvim";
    ".config/fastfetch".source = mkOutOfStoreSymlink "${configDir}/fastfetch";
    ".config/zathura".source = mkOutOfStoreSymlink "${configDir}/zathura";
    ".config/btop".source = mkOutOfStoreSymlink "${configDir}/btop";
    ".config/mpd".source = mkOutOfStoreSymlink "${configDir}/mpd";
    ".config/yt-dlp".source = mkOutOfStoreSymlink "${configDir}/yt-dlp";
    ".config/mpv".source = mkOutOfStoreSymlink "${configDir}/mpv";
    ".config/gowall".source = mkOutOfStoreSymlink "${configDir}/gowall";
    ".tridactylrc".source = mkOutOfStoreSymlink "${configDir}/tridactyl/tridactylrc";
    ".tmux.conf".source = mkOutOfStoreSymlink "${configDir}/tmux/.tmux.conf";
  };
}
