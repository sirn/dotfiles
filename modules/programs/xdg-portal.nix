{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;

  xdgDesktopPortalBin =
    if config.machine.isNixOS
    then "${pkgs.xdg-desktop-portal}/libexec/xdg-desktop-portal"
    else "/usr/libexec/xdg-desktop-portal";

  xdgDesktopPortalWlrBin =
    if config.machine.isNixOS
    then "${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr"
    else "/usr/libexec/xdg-desktop-portal-wlr";

  xdgDesktopPortalGtkBin =
    if config.machine.isNixOS
    then "${pkgs.xdg-desktop-portal-gtk}/libexec/xdg-desktop-portal-gtk"
    else "/usr/libexec/xdg-desktop-portal-gtk";
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

              run_and_disown ${xdgDesktopPortalWlrBin}
              run_and_disown ${xdgDesktopPortalGtkBin}
              run_and_disown ${xdgDesktopPortalBin} -vr
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
          default = "wlr";
          "org.freedesktop.impl.portal.AppChooser" = "gtk";
          "org.freedesktop.impl.portal.DynamicLauncher" = "gtk";
          "org.freedesktop.impl.portal.FileChooser" = "gtk";
          "org.freedesktop.impl.portal.Inhibit" = "gtk";
          "org.freedesktop.impl.portal.Notification" = "gtk";
          "org.freedesktop.impl.portal.Settings" = "gtk";
        };
      };
    };
  };
}
