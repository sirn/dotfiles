{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (lib) mkIf;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  home.packages = with pkgs; [
    docker-compose
    podman
    podman-compose
  ];

  home.file = mkIf isLinux {
    ".config/containers/storage.conf" = {
      source = mkOutOfStoreSymlink "${dotfilesDir}/etc/podman/storage.conf";
    };
  };
}
