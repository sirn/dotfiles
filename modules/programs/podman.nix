{ pkgs, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isDarwin isLinux;
in
{
  home.packages = with pkgs; [
    skopeo
  ] ++
  (if isLinux then [ podman ] else [ ]) ++
  (if isDarwin then [
    (pkgs.writeScriptBin "podman" ''
      #!${pkgs.bash}/bin/bash
      exec /usr/local/bin/docker "$@"
    '')
  ] else [ ]);
}
