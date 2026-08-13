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

  swaycfg = config.wayland.windowManager.sway;
in
{
  assertions = [
    {
      assertion = config.programs.noctalia.enable;
      message = "programs.noctalia must be enabled to use shell mode.";
    }
  ];

  programs.noctalia = {
    settings.theme.templates = {
      builtin_ids = [
        "gtk3"
        "gtk4"
        "kcolorscheme"
        "qt"
      ];
    };
  };

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
      name = "qtct";
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
