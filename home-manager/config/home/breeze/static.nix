{
  pkgs,
  config,
  lib,
  ...
}:

let
  breezePkg = pkgs.kdePackages.breeze;

  breezeGtkPkg = pkgs.kdePackages.breeze-gtk;

  breezeIconsPkg = pkgs.kdePackages.breeze-icons;

  gtkconf = config.gtk;

  swaycfg = config.wayland.windowManager.sway;

  # Breeze GTK theme name and icon theme name follow the desktop variant.
  isDark =
    config.home.colors.variants.desktop == "dark"
    || (
      config.home.colors.variants.desktop == "auto"
      && config.home.colors.variants.desktopFallback == "dark"
    );

  gtkThemeName = if isDark then "Breeze-Dark" else "Breeze";
  iconThemeName = if isDark then "breeze-dark" else "breeze";
  colorScheme = if isDark then "prefer-dark" else "prefer-light";
  kdeglobalsFile = if isDark then "BreezeDark.colors" else "BreezeLight.colors";
in
{
  gtk = {
    enable = pkgs.stdenv.isLinux;

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
      name = lib.mkDefault gtkThemeName;
      package = breezeGtkPkg;
    };

    iconTheme = {
      name = lib.mkDefault iconThemeName;
      package = breezeIconsPkg;
    };

    gtk2.extraConfig = ''
      gtk-alternative-button-order = 1;
    '';

    gtk3.extraConfig = {
      gtk-alternative-button-order = 1;
    };

    gtk4.extraConfig = {
      document-font-name = "sans-serif 10";
      monospace-font-name = "monospace 10";
    };
  };

  qt = {
    # Setting qt platformTheme and style via Home Manager on non-NixOS
    # can cause SEGFAULT due to dependency mismatch.
    enable = pkgs.stdenv.isLinux && !config.targets.genericLinux.enable;

    platformTheme = {
      name = "kde";
    };

    style = {
      name = "breeze";
      package = breezePkg;
    };
  };

  xdg.configFile."kdeglobals".source = "${breezePkg}/share/color-schemes/${kdeglobalsFile}";

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
          if pkgs.stdenv.isLinux && !config.targets.genericLinux.enable then
            "${pkgs.glib.bin}/bin/gsettings"
          else
            "/usr/bin/gsettings";

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
    sessionVariables = lib.mkIf (pkgs.stdenv.isLinux && config.targets.genericLinux.enable) {
      QT_QPA_PLATFORMTHEME = config.qt.platformTheme.name;
      QT_STYLE_OVERRIDE = config.qt.style.name;
    };
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      seat = {
        "*" = {
          xcursor_theme = "${config.gtk.cursorTheme.name} ${toString config.gtk.cursorTheme.size}";
        };
      };
    };
  };
}
