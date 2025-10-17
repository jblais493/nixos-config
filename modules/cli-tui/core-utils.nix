{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Modern CLI replacements
    eza # ls → eza
    bat # cat → bat
    fd # find → fd
    ripgrep-all # grep → rg
    zoxide # cd → z
    fzf # fuzzy finder

    # File management
    yazi # terminal file manager
    tree # directory visualization

    # Archive tools
    zip
    unzip

    # Essential utilities
    jq # JSON processor
    rsync # file sync
    coreutils # GNU core utilities
  ];
}
