{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  home.packages = with pkgs; if isDarwin then [ ] else [
    local.s-tui
  ];
}
