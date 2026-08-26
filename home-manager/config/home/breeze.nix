{
  pkgs,
  config,
  lib,
  ...
}:

let
  gtkconf = config.gtk;

  swaycfg = config.wayland.windowManager.sway;

  swaymsgBin = "${swaycfg.package}/bin/swaymsg";

  noctaliacfg = config.programs.noctalia;

  # ----------------------------------------------------
  # Base
  # ----------------------------------------------------

  breezePkg = pkgs.kdePackages.breeze;

  # ----------------------------------------------------
  # GTK
  # ----------------------------------------------------

  breezeGtkThemePkg = pkgs.kdePackages.breeze-gtk;

  breezeGtkThemeName = {
    dark = "Breeze-Dark";
    light = "Breeze";
  };

  # ----------------------------------------------------
  # Icons
  # ----------------------------------------------------

  breezeIconsPkg = pkgs.kdePackages.breeze-icons;

  breezeIconsThemeName = {
    dark = "breeze-dark";
    light = "breeze";
  };

  # ----------------------------------------------------
  # Cursors
  # ----------------------------------------------------

  breezeCursorPkg = breezePkg;

  breezeCursorThemeName = {
    dark = "breeze_cursors";
    light = "breeze_cursors";
  };

  breezeColorSchemeFile = {
    dark = "${breezePkg}/share/color-schemes/BreezeDark.colors";
    light = "${breezePkg}/share/color-schemes/BreezeLight.colors";
  };

  # ----------------------------------------------------
  # Default
  # ----------------------------------------------------

  defaultColorSchemeName =
    if
      config.home.colors.variants.desktop == "dark"
      || (
        config.home.colors.variants.desktop == "auto"
        && config.home.colors.variants.desktopFallback == "dark"
      )
    then
      "dark"
    else
      "light";

  # ----------------------------------------------------
  # Script
  # ----------------------------------------------------

  gsettingsBin =
    if pkgs.stdenv.isLinux && !config.targets.genericLinux.enable then
      "${pkgs.glib.bin}/bin/gsettings"
    else
      "/usr/bin/gsettings";

  gsettingsDesktopSchemas = pkgs.gsettings-desktop-schemas;

  updateBreezeAppearance = pkgs.writeScriptBin "update-breeze-appearance" ''
    # ----------------------------------------------------
    # GTK
    # ----------------------------------------------------

    _gsettings() {
      XDG_DATA_DIRS="${gsettingsDesktopSchemas}/share/gsettings-schemas/${gsettingsDesktopSchemas.name}:$XDG_DATA_DIRS"
      ${gsettingsBin} "$@" || true
    }

    _setGtkCommon() {
      _gsettings set org.gnome.desktop.interface cursor-size ${toString gtkconf.cursorTheme.size}
      _gsettings set org.gnome.desktop.interface document-font-name "${gtkconf.font.name} ${toString gtkconf.font.size}"
      _gsettings set org.gnome.desktop.interface font-name "${gtkconf.font.name} ${toString gtkconf.font.size}"
      _gsettings set org.gnome.desktop.interface monospace-font-name "monospace 10"
    }

    setGtkLightTheme() {
      _setGtkCommon
      _gsettings set org.gnome.desktop.interface color-scheme prefer-light
      _gsettings set org.gnome.desktop.interface cursor-theme "${breezeCursorThemeName.light}"
      _gsettings set org.gnome.desktop.interface gtk-theme "${breezeGtkThemeName.light}"
      _gsettings set org.gnome.desktop.interface icon-theme "${breezeIconsThemeName.light}"
    }

    setGtkDarkTheme() {
      _setGtkCommon
      _gsettings set org.gnome.desktop.interface color-scheme prefer-dark
      _gsettings set org.gnome.desktop.interface cursor-theme "${breezeCursorThemeName.dark}"
      _gsettings set org.gnome.desktop.interface gtk-theme "${breezeGtkThemeName.dark}"
      _gsettings set org.gnome.desktop.interface icon-theme "${breezeIconsThemeName.dark}"
    }

    # ----------------------------------------------------
    # KDE
    # ----------------------------------------------------

    ${lib.optionalString config.systemd.user.enable ''
      _restartXdpKde() {
        # Hack; xdg-desktop-portal-kde only set theme at the start.
        ${lib.getExe' pkgs.systemd "systemctl"} restart --user plasma-xdg-desktop-portal-kde || true
      }
    ''}

    setKdeLightTheme() {
      XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
      cp -f "${breezeColorSchemeFile.light}" "$XDG_CONFIG_HOME/kdeglobals"
      ${lib.optionalString config.systemd.user.enable "_restartXdpKde"}
    }

    setKdeDarkTheme() {
      XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}
      cp -f "${breezeColorSchemeFile.dark}" "$XDG_CONFIG_HOME/kdeglobals"
      ${lib.optionalString config.systemd.user.enable "_restartXdpKde"}
    }

    ${lib.optionalString swaycfg.enable (
      let
        cursorSize = toString config.gtk.cursorTheme.size;
      in
      ''
        # ----------------------------------------------------
        # Sway
        # ----------------------------------------------------

        setSwayLightTheme() {
          if [ -n "''${SWAYSOCK-}" ] && [ -S "$SWAYSOCK" ]; then
            ${swaymsgBin} 'seat * xcursor_theme ${breezeCursorThemeName.light} ${cursorSize}'
          fi
        }

        setSwayDarkTheme() {
          if [ -n "''${SWAYSOCK-}" ] && [ -S "$SWAYSOCK" ]; then
            ${swaymsgBin} 'seat * xcursor_theme ${breezeCursorThemeName.dark} ${cursorSize}'
          fi
        }
      ''
    )}

    # ----------------------------------------------------
    # Entrypoint
    # ----------------------------------------------------

    entrypoint() {
      THEME=''${1:-${defaultColorSchemeName}}

      if [ "$THEME" = "dark" ]; then
        setGtkDarkTheme
        setKdeDarkTheme
        ${lib.optionalString swaycfg.enable "setSwayDarkTheme"}
      else
        setGtkLightTheme
        setKdeLightTheme
        ${lib.optionalString swaycfg.enable "setSwayLightTheme"}
      fi
    }

    entrypoint "$@"
  '';

  # ----------------------------------------------------
  # Hooks
  # ----------------------------------------------------

  # When Noctalia is enabled, Noctalia will handle the switch.
  needsActivationScript = !noctaliacfg.enable || config.home.colors.variants.desktop != "auto";
in
{
  gtk = {
    enable = pkgs.stdenv.isLinux;

    cursorTheme = {
      name = lib.mkDefault breezeCursorThemeName.${defaultColorSchemeName};
      package = breezeCursorPkg;
      size = 24;
    };

    font = {
      name = "sans-serif";
      size = 10;
    };

    theme = {
      name = lib.mkDefault breezeGtkThemeName.${defaultColorSchemeName};
      package = breezeGtkThemePkg;
    };

    iconTheme = {
      name = lib.mkDefault breezeIconsThemeName.${defaultColorSchemeName};
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

  home = {
    packages = with pkgs; [
      breezeCursorPkg
      breezeGtkThemePkg
      breezeIconsPkg
      breezePkg

      # fallback
      hicolor-icon-theme
    ];

    activation = lib.mkIf needsActivationScript {
      setupBreezeAppearance = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        ${lib.getExe updateBreezeAppearance}
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

  programs.noctalia = lib.mkIf noctaliacfg.enable {
    settings.hooks = {
      theme_mode_changed = "${lib.getExe updateBreezeAppearance} $NOCTALIA_THEME_MODE";
    };
  };

  flatpak.globalOverrides = lib.mkIf config.flatpak.enable {
    environment = {
      QT_QPA_PLATFORMTHEME = "xdgdesktopportal";
    };
  };
}
