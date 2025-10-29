{ config, lib, pkgs, ... }:
{
  # Terminal appearance - Nord theme
  terminal.colors = {
    background = "#2e3440";
    foreground = "#d8dee9";
    cursor = "#d8dee9";

    # Normal colors
    color0 = "#3b4252";  # black
    color1 = "#bf616a";  # red
    color2 = "#a3be8c";  # green
    color3 = "#ebcb8b";  # yellow
    color4 = "#81a1c1";  # blue
    color5 = "#b48ead";  # magenta
    color6 = "#88c0d0";  # cyan
    color7 = "#e5e9f0";  # white

    # Bright colors
    color8 = "#4c566a";   # bright black
    color9 = "#bf616a";   # bright red
    color10 = "#a3be8c";  # bright green
    color11 = "#ebcb8b";  # bright yellow
    color12 = "#81a1c1";  # bright blue
    color13 = "#b48ead";  # bright magenta
    color14 = "#8fbcbb";  # bright cyan
    color15 = "#eceff4";  # bright white
  };

  # Font configuration
  terminal.font = "${pkgs.nerdfonts.override { fonts = [ "GeistMono" ]; }}/share/fonts/truetype/NerdFonts/GeistMonoNerdFont-Regular.ttf";

  environment.packages = with pkgs; [
    # Editors
    vim
    neovim
    git

    # SSH
    openssh
    shadow
    gnupg

    # Core utilities
    coreutils
    procps
    iproute2
    nettools
    gnugrep
    which
    killall
    hostname
    man
    gzip

    # Power user tools
    tmux        # Terminal multiplexer - essential
    htop        # Better top
    fd          # Better find
    ripgrep     # Better grep
    fzf         # Fuzzy finder
    bat         # Better cat with syntax highlighting
    eza         # Better ls
    zoxide      # Smarter cd
    delta       # Better git diff

    # Fonts
    (nerdfonts.override { fonts = [ "GeistMono" ]; })
  ];

  # SSH daemon configuration
  build.activation.sshd-setup = ''
    mkdir -p $HOME/.ssh
    chmod 700 $HOME/.ssh

    if [ ! -f $HOME/.ssh/ssh_host_ed25519_key ]; then
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f $HOME/.ssh/ssh_host_ed25519_key -N ""
    fi
    if [ ! -f $HOME/.ssh/ssh_host_rsa_key ]; then
      ${pkgs.openssh}/bin/ssh-keygen -t rsa -f $HOME/.ssh/ssh_host_rsa_key -N ""
    fi
    chmod 600 $HOME/.ssh/ssh_host_*

    cat > $HOME/.ssh/sshd_config << 'EOF'
Port 8022
HostKey /data/data/com.termux.nix/files/home/.ssh/ssh_host_rsa_key
HostKey /data/data/com.termux.nix/files/home/.ssh/ssh_host_ed25519_key
PubkeyAuthentication yes
AuthorizedKeysFile .ssh/authorized_keys
PasswordAuthentication no
PermitRootLogin no
PidFile /data/data/com.termux.nix/files/home/.ssh/sshd.pid
EOF

    touch $HOME/.ssh/authorized_keys
    chmod 600 $HOME/.ssh/authorized_keys

    mkdir -p $HOME/.local/bin
    cat > $HOME/.local/bin/start-ssh << 'SCRIPT'
#!/usr/bin/env bash
${pkgs.openssh}/bin/sshd -f $HOME/.ssh/sshd_config
SCRIPT
    chmod +x $HOME/.local/bin/start-ssh
  '';

  environment.sessionVariables = {
    PATH = "$HOME/.local/bin:$PATH";
    EDITOR = "nvim";
    PAGER = "less";

    # Better defaults
    LESS = "-R";  # Allow colors in less
    BAT_THEME = "Nord";
  };

  environment.etcBackupExtension = ".bak";

  system.stateVersion = "24.05";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
