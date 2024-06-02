{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.desktop.enable {
  i18n.inputMethod = mkIf config.machine.isNixOS {
    enabled = "fcitx5";

    fcitx5 = {
      addons = with pkgs; [
        fcitx5-mozc
      ];
    };
  };

  wayland.windowManager.sway =
    {
      config = {
        seat = {
          "*" = {
            xcursor_theme = "${config.gtk.cursorTheme.name} ${toString config.gtk.cursorTheme.size}";
          };
        };
      };
    };

  # non-NixOS; assume no systemd
  wayexec.services =
    {
      fcitx5 = mkIf (config.i18n.inputMethod.enabled != "fcitx5") {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          fcitx5 -D -r
        '';
      };

      pipewire = mkIf (!config.machine.isNixOS) {
        runScript = ''
          #!${pkgs.execline}/bin/execlineb
          fdmove -c 2 1
          pipewire
        '';
      };
    };

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
        gtk-application-prefer-dark-theme = config.desktop.preferDark;
        gtk-alternative-button-order = 1;
      };

    gtk4.extraConfig =
      {
        color-scheme = if config.desktop.preferDark then "prefer-dark" else "default";
        document-font-name = "sans-serif 10";
        monospace-font-name = "monospace 10";
      };
  };

  # Setting qt platformTheme and style via Home Manager on non-NixOS
  # can cause SEGFAULT due to dependency mismatch.
  qt = {
    enable = config.machine.isNixOS;
    platformTheme = {
      name = "kde";
    };
    style = {
      name = "breeze";
    };
  };

  home = mkIf config.machine.isNixOS
    {
      packages = with pkgs; [
        breeze-qt5
        breeze-gtk
        breeze-icons
        hicolor-icon-theme
      ];
    } // (
    mkIf (!config.machine.isNixOS) {
      sessionVariables = {
        QT_QPA_PLATFORMTHEME = config.qt.platformTheme;
        QT_STYLE_OVERRIDE = config.qt.style.name;
      };
    });

  # This is necessary to get breeze-dark to apply for Qt applications
  xdg.configFile = {
    "kdeglobals" = mkIf config.desktop.preferDark {
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
}
