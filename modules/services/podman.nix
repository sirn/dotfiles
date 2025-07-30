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
  ];

  # On Linux, also enable socket activation
  xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
    "systemd/user/podman.socket".source = "${cfg.package}/share/systemd/user/podman.socket";
    "systemd/user/podman.service".source = "${cfg.package}/share/systemd/user/podman.service";
    "systemd/user/sockets.target.wants/podman.socket".source = "${cfg.package}/share/systemd/user/podman.socket";
  };

  # On Linux, also set DOCKER_HOST to use Podman socket
  home.sessionVariables = lib.mkIf (!config.machine.isNixOS) {
    "DOCKER_HOST" = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
  };
}
