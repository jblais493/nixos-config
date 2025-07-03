{ config, pkgs, ... }:

{
  # Install tmux
  home.packages = with pkgs; [ tmux ];

}
