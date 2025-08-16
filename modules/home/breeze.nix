{ pkgs, config, lib, ... }:

{
  gtk = {
    enable = true;
    cursorTheme.name = "breeze_cursors";
    cursorTheme.size = 24;
    font.name = "sans-serif";
    font.size = 10;
    theme.name = "Breeze";
    iconTheme.name = "Breeze";

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

      package = pkgs.kdePackages.breeze;
    };
  };

  home = {
    packages = with pkgs; [
      hicolor-icon-theme
      kdePackages.breeze
      kdePackages.breeze-gtk
      kdePackages.breeze-icons
    ];

    activation =
      let
        gsettingsBin =
          if config.machine.isNixOS
          then "${pkgs.glib.bin}/bin/gsettings"
          else "/usr/bin/gsettings";

        colorScheme =
          if config.machine.desktop.preferDark
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
          _gsettings set org.gnome.desktop.interface cursor-size 24
          _gsettings set org.gnome.desktop.interface cursor-theme "breeze_cursors"
          _gsettings set org.gnome.desktop.interface document-font-name "sans-serif 10"
          _gsettings set org.gnome.desktop.interface font-name "sans-serif 10"
          _gsettings set org.gnome.desktop.interface gtk-theme "Breeze"
          _gsettings set org.gnome.desktop.interface icon-theme "Breeze"
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

    file = lib.mkIf config.machine.desktop.preferDark {
      # This is necessary to get breeze-dark to apply to Qt applications
      ".config/kdeglobals" = {
        text = ''
          [ColorEffects:Disabled]
          ChangeSelectionColor=
          Color=56,56,56
          ColorAmount=0
          ColorEffect=0
          ContrastAmount=0.65
          ContrastEffect=1
          Enable=
          IntensityAmount=0.1
          IntensityEffect=2

          [ColorEffects:Inactive]
          ChangeSelectionColor=true
          Color=112,111,110
          ColorAmount=0.025
          ColorEffect=2
          ContrastAmount=0.1
          ContrastEffect=2
          Enable=false
          IntensityAmount=0
          IntensityEffect=0

          [Colors:Button]
          BackgroundAlternate=30,87,116
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Complementary]
          BackgroundAlternate=30,87,116
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Header]
          BackgroundAlternate=42,46,50
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Header][Inactive]
          BackgroundAlternate=49,54,59
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Selection]
          BackgroundAlternate=30,87,116
          BackgroundNormal=61,174,233
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=252,252,252
          ForegroundInactive=161,169,177
          ForegroundLink=253,188,75
          ForegroundNegative=176,55,69
          ForegroundNeutral=198,92,0
          ForegroundNormal=252,252,252
          ForegroundPositive=23,104,57
          ForegroundVisited=155,89,182

          [Colors:Tooltip]
          BackgroundAlternate=42,46,50
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:View]
          BackgroundAlternate=35,38,41
          BackgroundNormal=27,30,32
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Window]
          BackgroundAlternate=49,54,59
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [KDE]
          LookAndFeelPackage=org.kde.breezedark.desktop

          [WM]
          activeBackground=49,54,59
          activeBlend=252,252,252
          activeForeground=252,252,252
          inactiveBackground=42,46,50
          inactiveBlend=161,169,177
          inactiveForeground=161,169,177
        '';
      };
    };
  };
}
