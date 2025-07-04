{ config, pkgs, ... }:
{
  # Enable zsh programs
  programs.zsh.enable = true;

  # Set zsh as default shell for user
  users.users.joshua = {
    shell = pkgs.zsh;
  };

  # Install zsh and plugins (config managed via dotfiles)
  environment.systemPackages = with pkgs; [
    zsh
    oh-my-zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    starship
    thefuck
  ];
}
