{ pkgs, config, lib, ... }:

let
  preferDark = config.machine.desktop.preferDark;

  breezePkg = pkgs.kdePackages.breeze;

  breezeGtkPkg = pkgs.kdePackages.breeze-gtk;

  breezeIconsPkg = pkgs.kdePackages.breeze-icons;

  gtkconf = config.gtk;
in
{
  gtk = {
    enable = true;

    cursorTheme = {
      name = "breeze_cursors";
      package = breezePkg;
      size = 24;
    };

    font = {
      name = "sans-serif";
      size = 10;
    };

    theme = {
      name =
        if preferDark
        then "Breeze-Dark"
        else "Breeze";
      package = breezeGtkPkg;
    };

    iconTheme = {
      name =
        if preferDark
        then "breeze-dark"
        else "breeze";
      package = breezeIconsPkg;
    };

    gtk2.extraConfig = ''
      gtk-alternative-button-order = 1;
    '';

    gtk3.extraConfig =
      {
        gtk-alternative-button-order = 1;
      };

    gtk4.extraConfig =
      {
        document-font-name = "sans-serif 10";
        monospace-font-name = "monospace 10";
      };
  };

  qt = {
    # Setting qt platformTheme and style via Home Manager on non-NixOS
    # can cause SEGFAULT due to dependency mismatch.
    enable = config.machine.isNixOS;

    platformTheme = {
      name = "kde";
    };

    style = {
      name = "breeze";
      package = breezePkg;
    };
  };

  home = {
    packages = with pkgs; [
      breezeGtkPkg
      breezeIconsPkg
      breezePkg
      hicolor-icon-theme
    ];

    activation =
      let
        gsettingsBin =
          if config.machine.isNixOS
          then "${pkgs.glib.bin}/bin/gsettings"
          else "/usr/bin/gsettings";

        colorScheme =
          if preferDark
          then "prefer-dark"
          else "prefer-light";

        gsettingsDesktopSchemas = pkgs.gsettings-desktop-schemas;

        setupGnomeDesktopInterface = pkgs.writeScriptBin "setup-gnome-desktop-interface" ''
          #!${pkgs.runtimeShell}
          _gsettings() {
            XDG_DATA_DIRS="${gsettingsDesktopSchemas}/share/gsettings-schemas/${gsettingsDesktopSchemas.name}:$XDG_DATA_DIRS"
            ${gsettingsBin} "$@" || true
          }

          _gsettings set org.gnome.desktop.interface color-scheme ${colorScheme}
          _gsettings set org.gnome.desktop.interface cursor-size ${toString gtkconf.cursorTheme.size}
          _gsettings set org.gnome.desktop.interface cursor-theme "${gtkconf.cursorTheme.name}"
          _gsettings set org.gnome.desktop.interface document-font-name "${gtkconf.font.name} ${toString gtkconf.font.size}"
          _gsettings set org.gnome.desktop.interface font-name "${gtkconf.font.name} ${toString gtkconf.font.size}"
          _gsettings set org.gnome.desktop.interface gtk-theme "${gtkconf.theme.name}"
          _gsettings set org.gnome.desktop.interface icon-theme "${gtkconf.iconTheme.name}"
          _gsettings set org.gnome.desktop.interface monospace-font-name "monospace 10"
        '';
      in
      {
        setupBreeze = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          setupBreeze() {
            ${lib.getExe setupGnomeDesktopInterface}
          }

          setupBreeze
        '';
      };

    # On a non-NixOS, we just provide the proper environment variables
    # for it to pick up the correct themes installed with the system
    sessionVariables = lib.mkIf (!config.machine.isNixOS) {
      QT_QPA_PLATFORMTHEME = config.qt.platformTheme.name;
      QT_STYLE_OVERRIDE = config.qt.style.name;
    };
  };

  xdg.configFile = {
    "kdeglobals" = lib.mkIf preferDark {
      source = "${breezePkg}/share/color-schemes/BreezeDark.colors";
    };
  };
}
