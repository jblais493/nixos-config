{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    btop # system monitor
    trash-cli # safe deletion
    tldr # quick man pages
    fastfetch # system info
    killall # process management
    ydotool # automation
    borgbackup # backups

    gowall # theming
    tesseract # OCR image processing
  ];
}
