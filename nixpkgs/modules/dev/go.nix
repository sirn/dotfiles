{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    go
    golangci-lint
    gopls
  ];
}
