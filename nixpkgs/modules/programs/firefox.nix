{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  flatpak.applications = mkIf (isLinux && config.flatpak.enable) {
    "org.mozilla.firefox" = {
      overrides = {
        environment = {
          MOZ_ENABLE_WAYLAND = "1";
        };
      };
    };
  };
}
