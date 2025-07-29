{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    mu
    isync
    msmtp

    gnutls
  ];

  environment.sessionVariables = {
    EMACSLOADPATH = "${pkgs.emacsPackages.mu4e}/share/emacs/site-lisp/mu4e:";
  };
}
