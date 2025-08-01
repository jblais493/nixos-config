{ config, pkgs, ... }:
{
  # Install Emacs and Doom dependencies at system level
  environment.systemPackages = with pkgs; [
    emacs
    emacsPackages.mu4e
    # Doom Emacs dependencies
    ripgrep
    vips
    wmctrl
    fd
    imagemagick
    texlive.combined.scheme-full  # For LaTeX support
    sqlite       # For org-roam
    aspell
    aspellDicts.en
    aspellDicts.en-computers  # technical terms
    aspellDicts.en-science    # scientific terms

    # Additional tools Doom might need
    git
    curl
    wget

    # Additional Doom dependencies
    nodejs  # For LSP servers
    shellcheck  # For shell script checking
    html-tidy  # For HTML formatting
    stylelint  # For CSS linting

    # Fonts that Doom recommends
    emacs-all-the-icons-fonts
  ];

  # System-level Emacs configuration
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
    defaultEditor = true;  # Set Emacs as default editor
  };
}
