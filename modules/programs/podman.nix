{ pkgs, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isDarwin isLinux;
in
{
  home.packages = with pkgs; [
    skopeo
  ] ++
  (if isLinux then [
    podman
    podman-compose

    (pkgs.writeScriptBin "docker" ''
      #!${pkgs.bash}/bin/bash
      exec ${podman}/bin/podman "$@"
    '')

    (pkgs.writeScriptBin "docker-compose" ''
      #!${pkgs.bash}/bin/bash
      exec ${podman-compose}/bin/podman-compose "$@"
    '')
  ] else [ ]) ++
  (if isDarwin then [
    (pkgs.writeScriptBin "podman" ''
      #!${pkgs.bash}/bin/bash
      exec /usr/local/bin/docker "$@"
    '')

    (pkgs.writeScriptBin "podman-compose" ''
      #!${pkgs.bash}/bin/bash
      exec /usr/local/bin/docker-compose "$@"
    '')
  ] else [ ]);
}
