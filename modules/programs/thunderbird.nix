{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
mkIf config.machine.gui.enable {
  flatpak.applications = mkIf (isLinux && config.flatpak.enable) {
    "org.mozilla.Thunderbird" = {
      overrides = {
        environment = {
          MOZ_ENABLE_WAYLAND = "1";
        };
      };
    };
  };
}
