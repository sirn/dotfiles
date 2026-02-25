{ config, pkgs, lib, ... }:

let
  cfg = config.services.podman;
in
{
  services.podman = {
    enable = pkgs.stdenv.isLinux;
  };

  home.packages = with pkgs; [
    podman-compose
    skopeo
  ] ++ (
    if pkgs.stdenv.isDarwin
    then [ podman ]
    else [ ]
  );

  # On Linux, also set DOCKER_HOST to use Podman socket
  home.sessionVariables = lib.mkIf cfg.enable {
    "DOCKER_HOST" = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
  };
}
