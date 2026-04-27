{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Palette generator function passed to themes
  generatePalette =
    name: base16Colors:
    let
      palette240File =
        pkgs.runCommand "generate-palette-256-${name}" { nativeBuildInputs = [ pkgs.python3 ]; }
          ''
            cat > input.json <<'EOF'
            {
              "bg": "${base16Colors.background}",
              "fg": "${base16Colors.foreground}",
              "normal": [
                "${base16Colors.normal.black}",
                "${base16Colors.normal.red}",
                "${base16Colors.normal.green}",
                "${base16Colors.normal.yellow}",
                "${base16Colors.normal.blue}",
                "${base16Colors.normal.magenta}",
                "${base16Colors.normal.cyan}",
                "${base16Colors.normal.white}"
              ]
            }
            EOF
            python3 ${./generate-palette.py} < input.json > $out
          '';

      palette240 = builtins.fromJSON (builtins.readFile palette240File);

      base16List = [
        base16Colors.normal.black
        base16Colors.normal.red
        base16Colors.normal.green
        base16Colors.normal.yellow
        base16Colors.normal.blue
        base16Colors.normal.magenta
        base16Colors.normal.cyan
        base16Colors.normal.white
        base16Colors.bright.black
        base16Colors.bright.red
        base16Colors.bright.green
        base16Colors.bright.yellow
        base16Colors.bright.blue
        base16Colors.bright.magenta
        base16Colors.bright.cyan
        base16Colors.bright.white
      ];
    in
    base16List ++ palette240;

  # Auto-discover theme files from schemes directory
  themeFiles = builtins.readDir ./schemes;
  themeNames = builtins.map (lib.removeSuffix ".nix") (
    builtins.attrNames (
      lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".nix" name) themeFiles
    )
  );

  # Import all discovered themes
  themes = lib.genAttrs themeNames (
    name: import (./schemes + "/${name}.nix") { inherit lib generatePalette; }
  );

  # Selected theme for single-active consumers
  themeName = config.home.colors.themeName;
  theme = themes.${themeName};

  base16Colors = theme.base16Colors;
  variant = theme.variant;
  semantic = theme.semantic;
  palette256 = theme.palette256;

  stripHash = color: builtins.substring 1 6 color;
  colorScheme = base16Colors;

  # Helper to convert a theme record to a WezTerm color scheme
  weztermColorScheme = t: {
    ansi = [
      t.base16Colors.normal.black
      t.base16Colors.normal.red
      t.base16Colors.normal.green
      t.base16Colors.normal.yellow
      t.base16Colors.normal.blue
      t.base16Colors.normal.magenta
      t.base16Colors.normal.cyan
      t.base16Colors.normal.white
    ];
    brights = [
      t.base16Colors.bright.black
      t.base16Colors.bright.red
      t.base16Colors.bright.green
      t.base16Colors.bright.yellow
      t.base16Colors.bright.blue
      t.base16Colors.bright.magenta
      t.base16Colors.bright.cyan
      t.base16Colors.bright.white
    ];
    indexed = lib.listToAttrs (
      lib.imap0 (i: c: {
        name = toString i;
        value = c;
      }) t.palette256
    );
    background = t.base16Colors.background;
    cursor_bg = t.base16Colors.foreground;
    cursor_border = t.base16Colors.foreground;
    cursor_fg = t.base16Colors.background;
    foreground = t.base16Colors.foreground;
    scrollbar_thumb = t.base16Colors.scrollbar;
    selection_bg = t.base16Colors.selection;
    selection_fg = t.base16Colors.background;
  };

  # Helper to convert a theme record to a Ghostty theme
  ghosttyTheme = t: {
    background = stripHash t.base16Colors.background;
    cursor-color = stripHash t.base16Colors.foreground;
    foreground = stripHash t.base16Colors.foreground;
    palette = lib.imap0 (i: c: "${toString i}=${stripHash c}") t.palette256;
    selection-background = stripHash t.base16Colors.selection;
    selection-foreground = stripHash t.base16Colors.background;
  };

in
{
  home.colors.variant = variant;

  programs.emacs = lib.mkIf config.programs.emacs.enable {
    afterInitExtra = theme.emacsTheme.customElisp;
    themePackages = theme.emacsTheme.packages;
  };

  programs.alacritty = lib.mkIf config.programs.alacritty.enable {
    settings.colors = {
      primary = {
        background = colorScheme.background;
        foreground = colorScheme.foreground;
      };
      normal = colorScheme.normal;
      bright = colorScheme.bright;
    };
  };

  programs.foot = lib.mkIf config.programs.foot.enable {
    settings.colors = {
      background = stripHash colorScheme.background;
      foreground = stripHash colorScheme.foreground;
    }
    // lib.listToAttrs (
      lib.imap0 (i: c: {
        name = toString i;
        value = stripHash c;
      }) palette256
    );
  };

  programs.wezterm = lib.mkIf config.programs.wezterm.enable {
    colorSchemes = lib.mapAttrs (name: weztermColorScheme) themes;
  };

  programs.ghostty = lib.mkIf config.programs.ghostty.enable {
    settings.theme = themeName;
    themes = lib.mapAttrs (name: ghosttyTheme) themes;
  };

  wayland.windowManager.sway = lib.mkIf config.wayland.windowManager.sway.enable {
    config.colors = {
      focused = {
        background = "${semantic.focus.bg}99";
        border = "${semantic.focus.bg}99";
        childBorder = "${semantic.focus.bg}99";
        indicator = "${semantic.focus.bg}a5";
        text = semantic.focus.text;
      };
      focusedInactive = {
        background = "${semantic.inactive.bg}99";
        border = "${semantic.inactive.bg}a5";
        childBorder = "${semantic.inactive.bg}66";
        indicator = "${semantic.inactive.bg}50";
        text = semantic.inactive.text;
      };
      unfocused = {
        background = "${semantic.primary.bg}99";
        border = "${semantic.primary.bg}a5";
        childBorder = "${semantic.primary.bg}66";
        indicator = "${semantic.primary.bg}ff";
        text = semantic.primary.text;
      };
      placeholder = {
        background = semantic.warning.bg;
        border = semantic.warning.bg;
        childBorder = semantic.warning.bg;
        indicator = semantic.warning.bg;
        text = semantic.warning.text;
      };
      urgent = {
        background = semantic.urgent.bg;
        border = semantic.urgent.bg;
        childBorder = semantic.urgent.bg;
        indicator = semantic.urgent.bg;
        text = semantic.urgent.text;
      };
    };
  };

  programs.niri = lib.mkIf config.programs.niri.enable {
    settings.layout.focus-ring = {
      active.color = "${semantic.focus.bg}99";
      inactive.color = "${semantic.inactive.bg}99";
      urgent.color = semantic.urgent.bg;
    };
  };

  programs.waybar = lib.mkIf config.programs.waybar.enable {
    style = lib.mkDefault ''
      @define-color default_bg ${semantic.primary.bg};
      @define-color default_text ${semantic.primary.text};
      @define-color highlight_bg ${semantic.focus.bg};
      @define-color highlight_text ${semantic.focus.text};
      @define-color alert_bg ${semantic.urgent.bg};
      @define-color alert_text ${semantic.primary.text};
      @define-color battery_charging_bg ${semantic.battery.charging.bg};
      @define-color battery_charging_text ${semantic.battery.charging.text};
      @define-color battery_warning_bg ${semantic.battery.low.bg};
      @define-color battery_warning_text ${semantic.battery.low.text};
      @define-color battery_critical_bg ${semantic.battery.critical.bg};
      @define-color battery_critical_text ${semantic.battery.critical.text};
      @define-color inactive_bg ${semantic.inactive.bg};
      @define-color inactive_text ${semantic.inactive.text};

      ${builtins.readFile ./waybar.css}
    '';
  };

  programs.fuzzel = lib.mkIf config.programs.fuzzel.enable {
    settings.colors = {
      background = "${semantic.primary.bg}fa";
      selection = "${semantic.focus.bg}ff";
      border = "${semantic.focus.bg}99";
      text = "${semantic.primary.text}ff";
      match = "${semantic.primary.text}ff";
      selection-text = "${semantic.focus.text}ff";
      selection-match = "${semantic.focus.text}ff";
    };
  };

  programs.swaylock = lib.mkIf config.programs.swaylock.enable {
    settings.color = semantic.primary.bg;
  };

  services.mako = lib.mkIf config.services.mako.enable {
    settings = {
      background-color = "${semantic.primary.bg}fa";
      text-color = semantic.primary.text;
      border-color = "${semantic.focus.bg}99";
      progress-color = "${semantic.focus.bg}55";

      "urgency=low" = {
        border-color = semantic.inactive.bg;
      };

      "urgency=critical" = {
        border-color = semantic.urgent.bg;
        default-timeout = 0;
      };
    };
  };

  programs.omniwm = lib.mkIf config.programs.omniwm.enable {
    borders.color = "${semantic.focus.bg}99";
  };
}
