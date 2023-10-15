{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  flatpak.applications = mkIf (isLinux && config.flatpak.enable) {
    "md.obsidian.Obsidian" = {
      overrides = {
        sockets = [
          "wayland"
        ];

        environment = {
          OBSIDIAN_USE_WAYLAND = "1";
        };
      };
    };
  };
}
