{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  home.packages = with pkgs; [
    qemu
  ] ++ (if isDarwin then [
    local.lima-bin
  ] else [ ]);
}
