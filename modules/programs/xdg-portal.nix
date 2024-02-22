{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;

  joinedPortals = pkgs.buildEnv {
    name = "xdg-portals";

    paths = with pkgs; [
      local.xdg-desktop-portal-kde
      xdg-desktop-portal-wlr
      xdg-desktop-portal-gtk
    ];

    pathsToLink = [
      "/share/xdg-desktop-portal/portals"
      "/share/applications"
    ];
  };
in
{
  systemd.user.services = mkIf config.machine.isNixOS {
    xdg-desktop-portal-wlr = {
      Unit = {
        Description = "XDG Desktop Portal service for Wlroots";
        PartOf = "graphical-session.target";
      };
      Service = {
        ExecStart = "${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr";
      };
    };
    xdg-desktop-portal-kde = {
      Unit = {
        Description = "XDG Desktop Portal service for KDE";
        PartOf = "graphical-session.target";
      };
      Service = {
        ExecStart = "${pkgs.local.xdg-desktop-portal-kde}/libexec/xdg-desktop-portal-kde";
      };
    };
    xdg-desktop-portal-gtk = {
      Unit = {
        Description = "XDG Desktop Portal service for GTK";
        PartOf = "graphical-session.target";
      };
      Service = {
        ExecStart = "${pkgs.xdg-desktop-portal-gtk}/libexec/xdg-desktop-portal-gtk";
      };
    };
    xdg-desktop-portal = {
      Unit = {
        Description = "XDG Desktop Portal service";
        PartOf = "graphical-session.target";
        Requires = "xdg-desktop-portal-kde.service xdg-desktop-portal-wlr.service xdg-desktop-portal-gtk.service";
      };

      Service = {
        Environment = [
          "XDG_DESKTOP_PORTAL_DIR=${joinedPortals}/share/xdg-desktop-portal/portals"
        ];

        Type = "dbus";
        BusName = "org.freedesktop.portal.Desktop";
        ExecStart = "${pkgs.xdg-desktop-portal}/libexec/xdg-desktop-portal";
        Slice = "session.slice";
      };
    };
  };

  wayexec.services =
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

  home.file = {
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
