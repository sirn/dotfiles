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
            background = '${semantic.primary.bg}',
            new_tab = {
              bg_color = '${semantic.primary.bg}',
              fg_color = '${semantic.primary.text}',
            },
            new_tab_hover = {
              bg_color = '${semantic.primary.bg}',
              fg_color = '${semantic.hover.bg}',
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
          active_index_bg = '${semantic.focus.bg}',
          active_index_fg = '${semantic.focus.text}',
          active_title_bg = '${semantic.selection.bg}',
          active_title_fg = '${semantic.primary.text}',

          inactive_index_bg = '${semantic.inactive.bg}',
          inactive_index_fg = '${semantic.inactive.text}',
          inactive_title_bg = '${semantic.primary.bg}',
          inactive_title_fg = '${semantic.primary.text}',

          status_bg = '${semantic.inactive.bg}',
          status_fg = '${semantic.inactive.text}',
          status_icon_bg = '${semantic.primary.bg}',
          status_icon_fg = '${semantic.focus.bg}',
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
      @define-color default_bg_solid ${semantic.primary.bg};
      @define-color default_bg alpha(@default_bg_solid, 0.6);
      @define-color default_text ${semantic.primary.text};
      @define-color highlight_bg ${semantic.focus.bg};
      @define-color highlight_text ${semantic.primary.text};
      @define-color alert_bg ${semantic.urgent.bg};
      @define-color alert_text ${semantic.primary.text};
      @define-color battery_charging_bg ${semantic.battery.charging.bg};
      @define-color battery_charging_text ${semantic.battery.charging.text};
      @define-color battery_warning_bg ${semantic.battery.low.bg};
      @define-color battery_warning_text ${semantic.battery.low.text};
      @define-color battery_critical_bg ${semantic.battery.critical.bg};
      @define-color battery_critical_text ${semantic.battery.critical.text};
      @define-color muted_text ${semantic.inactive.text};

      ${builtins.readFile ./waybar.css}
    '';
  };

  programs.fuzzel = lib.mkIf config.programs.fuzzel.enable {
    settings.colors = {
      background = "${semantic.primary.bg}fa";
      selection = "${semantic.focus.bg}ff";
      border = "${semantic.scrollbar.bg}ff";
      text = "${semantic.primary.text}ff";
      match = "${semantic.primary.text}ff";
      selection-text = "${semantic.focus.text}ff";
      selection-match = "${semantic.focus.text}ff";
    };
  };

  programs.tmux.extraConfig = lib.mkIf config.programs.tmux.enable (
    lib.mkBefore ''
      set -g @color_primary_bg "${semantic.primary.bg}"
      set -g @color_primary_text "${semantic.primary.text}"
      set -g @color_inactive_bg "${semantic.inactive.bg}"
      set -g @color_inactive_text "${semantic.inactive.text}"
      set -g @color_selection_bg "${semantic.selection.bg}"
      set -g @color_focus_bg "${semantic.focus.bg}"
      set -g @color_focus_text "${semantic.focus.text}"
    ''
  );

  programs.swaylock = lib.mkIf config.programs.swaylock.enable {
    settings.color = semantic.primary.bg;
  };

  programs.starship.settings = lib.mkIf config.programs.starship.enable {
    hostname.style = semantic.primary.text;
    directory.style = semantic.hover.bg;
    git_branch.style = semantic.important.bg;
    git_status.style = semantic.important.bg;
    custom.jj.style = semantic.important.bg;
    nix_shell.style = semantic.success.bg;
    status.style = semantic.urgent.bg;
    character = {
      success_symbol = "[\\$](bold ${semantic.primary.text})";
      error_symbol = "[\\$](bold ${semantic.primary.text})";
    };
  };
}
