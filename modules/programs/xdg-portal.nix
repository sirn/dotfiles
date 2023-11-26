{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  wayland.windowManager.sway = mkIf (!config.machine.isNixOS) {
    config = {
      startup = [
        {
          command = ''
            ${pkgs.writeScriptBin "start-xdg-portals" ''
              #!${pkgs.bash}/bin/bash
              pkill -Af xdg-desktop-portal

              run_and_disown() {
                "$@" &
                sleep 0.5
                disown
              }

              run_and_disown /usr/libexec/xdg-desktop-portal-wlr
              run_and_disown /usr/libexec/xdg-desktop-portal-gtk
              run_and_disown /usr/libexec/xdg-desktop-portal -vr
            ''}/bin/start-xdg-portals
          '';
        }
      ];
    };
  };

  home.file = {
    ".config/xdg-desktop-portal/portals.conf" = {
      text = lib.generators.toINI { } {
        preferred = {
          default = "gtk";
          "org.freedesktop.impl.portal.Screencast" = "wlr";
          "org.freedesktop.impl.portal.Screenshot" = "wlr";
        };
      };
    };
  };
}
