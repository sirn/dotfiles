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

  # Import theme with palette generator
  theme = import ./${config.home.colors.themeName}.nix { inherit lib generatePalette; };

  base16Colors = theme.base16Colors;
  variant = theme.variant;
  semantic = theme.semantic;
  palette256 = theme.palette256;

  stripHash = color: builtins.substring 1 6 color;
  colorScheme = base16Colors;
in
{
  home.colors.variant = variant;

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
    colorSchemes.modus-vivendi = {
      ansi = [
        colorScheme.normal.black
        colorScheme.normal.red
        colorScheme.normal.green
        colorScheme.normal.yellow
        colorScheme.normal.blue
        colorScheme.normal.magenta
        colorScheme.normal.cyan
        colorScheme.normal.white
      ];
      brights = [
        colorScheme.bright.black
        colorScheme.bright.red
        colorScheme.bright.green
        colorScheme.bright.yellow
        colorScheme.bright.blue
        colorScheme.bright.magenta
        colorScheme.bright.cyan
        colorScheme.bright.white
      ];
      indexed = lib.listToAttrs (
        lib.imap0 (i: c: {
          name = toString i;
          value = c;
        }) palette256
      );
      background = colorScheme.background;
      cursor_bg = colorScheme.foreground;
      cursor_border = colorScheme.foreground;
      cursor_fg = colorScheme.background;
      foreground = colorScheme.foreground;
      scrollbar_thumb = colorScheme.scrollbar;
      selection_bg = colorScheme.selection;
      selection_fg = colorScheme.background;
    };
  };

  xdg.configFile."wezterm/modules/colors.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        color_scheme = 'modus-vivendi',
        colors = {
          tab_bar = {
            background = '${semantic.background}',
            new_tab = {
              bg_color = '${semantic.background}',
              fg_color = '${semantic.primaryText}',
            },
            new_tab_hover = {
              bg_color = '${semantic.background}',
              fg_color = '${semantic.hover}',
            },
          },
        },
      }
    '';
  };

  xdg.configFile."wezterm/hm_colors.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        tab_colors = {
          active_index_bg = '${semantic.focus}',
          active_index_fg = '${semantic.primaryText}',
          active_title_bg = '${semantic.selection}',
          active_title_fg = '${semantic.primaryText}',

          inactive_index_bg = '${semantic.inactiveBg}',
          inactive_index_fg = '${semantic.inactiveFg}',
          inactive_title_bg = '${semantic.background}',
          inactive_title_fg = '${semantic.inactiveFg}',

          status_bg = '${semantic.inactiveBg}',
          status_fg = '${semantic.inactiveFg}',
          status_icon_bg = '${semantic.background}',
          status_icon_fg = '${semantic.focus}',
        },
      }
    '';
  };

  programs.ghostty = lib.mkIf config.programs.ghostty.enable {
    settings.theme = "modus-vivendi";
    themes.modus-vivendi = {
      background = stripHash colorScheme.background;
      cursor-color = stripHash colorScheme.foreground;
      foreground = stripHash colorScheme.foreground;
      palette = lib.imap0 (i: c: "${toString i}=${stripHash c}") palette256;
      selection-background = stripHash colorScheme.selection;
      selection-foreground = stripHash colorScheme.background;
    };
  };

  wayland.windowManager.sway = lib.mkIf config.wayland.windowManager.sway.enable {
    config.colors = {
      focused = {
        background = "${semantic.focus}99";
        border = "${semantic.focus}99";
        childBorder = "${semantic.focus}99";
        indicator = "${semantic.focus}a5";
        text = semantic.accentText;
      };
      focusedInactive = {
        background = "${semantic.inactiveBg}99";
        border = "${semantic.inactiveBg}a5";
        childBorder = "${semantic.inactiveBg}66";
        indicator = "${semantic.inactiveBg}50";
        text = semantic.inactiveFg;
      };
      unfocused = {
        background = "${semantic.background}99";
        border = "${semantic.background}a5";
        childBorder = "${semantic.background}66";
        indicator = "${semantic.background}ff";
        text = semantic.primaryText;
      };
      placeholder = {
        background = semantic.warning;
        border = semantic.warning;
        childBorder = semantic.warning;
        indicator = semantic.warning;
        text = semantic.primaryText;
      };
      urgent = {
        background = semantic.urgent;
        border = semantic.urgent;
        childBorder = semantic.urgent;
        indicator = semantic.urgent;
        text = semantic.accentText;
      };
    };
  };

  programs.niri = lib.mkIf config.programs.niri.enable {
    settings.layout.focus-ring = {
      active.color = "${semantic.focus}99";
      inactive.color = "${semantic.inactiveBg}99";
      urgent.color = semantic.urgent;
    };
  };

  programs.waybar = lib.mkIf config.programs.waybar.enable {
    style = lib.mkDefault ''
      @define-color default_bg_solid ${semantic.background};
      @define-color default_bg alpha(@default_bg_solid, 0.6);
      @define-color default_fg ${semantic.primaryText};
      @define-color highlight_bg ${semantic.focus};
      @define-color highlight_fg ${semantic.background};
      @define-color alert_bg ${semantic.urgent};
      @define-color alert_fg ${semantic.accentText};
      @define-color battery_charging_bg ${semantic.battery.charging.bg};
      @define-color battery_charging_fg ${semantic.battery.charging.fg};
      @define-color battery_warning_bg ${semantic.battery.low.bg};
      @define-color battery_warning_fg ${semantic.battery.low.fg};
      @define-color battery_critical_bg ${semantic.battery.critical.bg};
      @define-color battery_critical_fg ${semantic.battery.critical.fg};
      @define-color muted_text ${semantic.inactiveFg};

      ${builtins.readFile ./waybar.css}
    '';
  };

  programs.fuzzel = lib.mkIf config.programs.fuzzel.enable {
    settings.colors = {
      background = "${semantic.background}fa";
      selection = "${semantic.selection}ff";
      border = "${semantic.scrollbar}ff";
      text = "${semantic.inactiveFg}ff";
      match = "${semantic.primaryText}ff";
      selection-text = "${semantic.primaryText}ff";
      selection-match = "${semantic.primaryText}ff";
    };
  };

  programs.swaylock = lib.mkIf config.programs.swaylock.enable {
    settings.color = semantic.background;
  };

  programs.starship.settings = lib.mkIf config.programs.starship.enable {
    hostname.style = semantic.primaryText;
    directory.style = semantic.hover;
    git_branch.style = semantic.vcs;
    git_status.style = semantic.vcs;
    custom.jj.style = semantic.vcs;
    nix_shell.style = semantic.success;
    status.style = semantic.urgent;
    character = {
      success_symbol = "[\\$](bold ${semantic.primaryText})";
      error_symbol = "[\\$](bold ${semantic.primaryText})";
    };
  };
}
