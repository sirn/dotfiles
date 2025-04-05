{ config, lib, pkgs, ... }:

{
  home.file =
    # On NixOS, this is configured via system configuration.
    lib.mkIf (!config.machine.isNixOS) {
      ".config/xdg-desktop-portal/portals.conf" = {
        text = lib.generators.toINI { } {
          preferred = {
            default = "kde";
            "org.freedesktop.impl.portal.Screencast" = "wlr";
            "org.freedesktop.impl.portal.Screenshot" = "wlr";
            "org.freedesktop.impl.portal.Settings" = "kde;gtk";
          };
        };
      };
    };
}
