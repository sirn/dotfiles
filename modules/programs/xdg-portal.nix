{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;
in
{
  home.packages = with pkgs; [
    kdePackages.xdg-desktop-portal-kde
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-wlr
  ];

  home.file = {
    ".config/xdg-desktop-portal/portals.conf" = {
      text = lib.generators.toINI { } {
        preferred = {
          default = "kde";
          "org.freedesktop.impl.portal.Inhibit" = "none";
          "org.freedesktop.impl.portal.Settings" = "gtk";
        } // (if (swaycfg.enable || niricfg.enable) then {
          "org.freedesktop.impl.portal.Screencast" = "wlr";
          "org.freedesktop.impl.portal.Screenshot" = "wlr";
        } else { });
      };
    };
  };
}
