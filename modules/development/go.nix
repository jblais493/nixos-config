{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    godef
    go
    gopls # LSP server
    gotools # Includes goimports, guru, etc.
    gomodifytags # For struct tag manipulation
    gotests # Test generation
    gore # Go REPL (if you actually use it)
    golangci-lint # Linting
    delve # Debugger
  ];
}
