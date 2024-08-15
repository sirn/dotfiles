{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  wayexec.services =
    # On NixOS, this is configured via system configuration.
    mkIf (!config.machine.isNixOS) {
      xdg-desktop-portal-wlr = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          execline-cd ${config.home.homeDirectory}
          /usr/libexec/xdg-desktop-portal-wlr
        '';
      };
      xdg-desktop-portal-kde = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          /usr/libexec/xdg-desktop-portal-kde
        '';
      };
      xdg-desktop-portal-gtk = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          execline-cd ${config.home.homeDirectory}
          /usr/libexec/xdg-desktop-portal-gtk
        '';
      };
      xdg-desktop-portal = {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          if {
            redirfd -w 1 /dev/null
            env SVDIR=${config.home.homeDirectory}/${config.wayexec.serviceDir}
            if { sv check xdg-desktop-portal-wlr }
            if { sv check xdg-desktop-portal-kde }
            if { sv check xdg-desktop-portal-gtk }
          }
          fdmove -c 2 1
          execline-cd ${config.home.homeDirectory}

          # xdg-desktop-portal can sometimes start quickly enough that
          # xdg-desktop-portal-{wlr,kde,gtk} hasn't finished initializing.
          if { ${pkgs.coreutils}/bin/sleep 0.5 }
          /usr/libexec/xdg-desktop-portal -r
        '';
      };
    };

  home.file =
    # On NixOS, this is configured via system configuration.
    mkIf (!config.machine.isNixOS) {
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
