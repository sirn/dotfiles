{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  flatpak.applications = mkIf isLinux {
    "org.mozilla.Thunderbird" = {
      overrides = {
        environment = {
          MOZ_ENABLE_WAYLAND = "1";
        };
      };
    };
  };
}
