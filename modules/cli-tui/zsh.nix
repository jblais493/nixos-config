{ config, pkgs, ... }:
{
  # Enable zsh with built-in configuration
  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "sudo" "history" "fzf" ];
      theme = "robbyrussell";  # or any theme you prefer
    };
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;

    # Enable fzf integration
    enableFzfIntegration = true;

    # Optional: Add some aliases
    shellAliases = {
      ll = "ls -l";
      la = "ls -la";
      grep = "grep --color=auto";
    };
  };

  # Set zsh as default shell for user
  users.users.joshua.shell = pkgs.zsh;

  # Install additional tools (remove the zsh plugins from here)
  environment.systemPackages = with pkgs; [
    starship  # Modern prompt
    fzf       # Fuzzy finder (needed for fzf integration)
    fd        # Better find (used by fzf)
    ripgrep   # Better grep (used by fzf)
  ];
}
