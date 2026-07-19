{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.tiler;

  tomlFormat = pkgs.formats.toml { };

  keybindSubmodule = types.submodule {
    options = {
      keys = mkOption {
        type = types.str;
        description = "Key combination, e.g. \"ctrl+alt+h\".";
      };

      action = mkOption {
        type = types.str;
        description = "Action name. See tiler's config.toml.example for the available actions.";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Positional arguments for the action (e.g. workspace number).";
      };
    };
  };

  appRuleSubmodule = types.submodule {
    options = {
      app = mkOption {
        type = types.str;
        description = "Bundle identifier pattern matched against the app (`*` wildcard, case-insensitive).";
      };

      action = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Rule action, e.g. \"float\" to exclude the app from tiling.";
      };

      columnWidth = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = "Preferred width fraction (0.0-1.0) for windows of this app.";
      };
    };
  };

  # General/layout/workspaces options default to null so omitted fields fall
  # back to tiler's built-in defaults (ConfigLoader merges over Config.defaults).
  generalSubmodule = types.submodule {
    options = {
      outerGaps = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = "Gap between the screen edge and the outermost windows (points).";
      };

      innerGaps = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = "Gap between windows stacked vertically in the same column.";
      };

      columnGaps = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = "Gap between adjacent columns.";
      };

      minWindowWidth = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Minimum window width before tiling is skipped (points).";
      };

      minWindowHeight = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Minimum window height before tiling is skipped (points).";
      };

      focusColumnWrap = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "When true, focus_column_left/right wrap within the active screen instead of crossing displays.";
      };
    };
  };

  layoutSubmodule = types.submodule {
    options = {
      defaultColumnWidth = mkOption {
        type = types.nullOr types.float;
        default = null;
        description = "Width fraction (0.1-1.0) for newly-created columns.";
      };

      centerFocusedColumn = mkOption {
        type = types.nullOr (
          types.enum [
            "never"
            "on-overflow"
            "always"
          ]
        );
        default = null;
        description = ''
          When to scroll the viewport so the focused column is centred:
            "never"        scroll it fully into view (tiler default)
            "on-overflow"  centre it only when wider than the viewport
            "always"       always centre the focused column
        '';
      };

      alwaysCenterSingleColumn = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "When true, a workspace with a single column is always centred, regardless of center_focused_column. Mirrors Niri's always-center-single-column.";
      };
    };
  };

  workspacesSubmodule = types.submodule {
    options = {
      count = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Number of workspaces (1..count). Tiler default is 9.";
      };
    };
  };

  settingsSubmodule = types.submodule {
    options = {
      general = mkOption {
        type = generalSubmodule;
        default = { };
        description = "[general] section.";
      };

      layout = mkOption {
        type = layoutSubmodule;
        default = { };
        description = "[layout] section.";
      };

      workspaces = mkOption {
        type = workspacesSubmodule;
        default = { };
        description = "[workspaces] section.";
      };

      keybinds = mkOption {
        type = types.listOf keybindSubmodule;
        default = [ ];
        description = "List of [[keybind]] entries. Tiler ships no built-in keybinds, so this is the complete set.";
      };

      appRules = mkOption {
        type = types.listOf appRuleSubmodule;
        default = [ ];
        description = "List of [[app_rule]] per-application overrides.";
      };
    };
  };

  dropNulls = lib.filterAttrs (_: v: v != null);

  generalSection = dropNulls {
    outer_gaps = cfg.settings.general.outerGaps;
    inner_gaps = cfg.settings.general.innerGaps;
    column_gaps = cfg.settings.general.columnGaps;
    min_window_width = cfg.settings.general.minWindowWidth;
    min_window_height = cfg.settings.general.minWindowHeight;
    focus_column_wrap = cfg.settings.general.focusColumnWrap;
  };

  layoutSection = dropNulls {
    default_column_width = cfg.settings.layout.defaultColumnWidth;
    center_focused_column = cfg.settings.layout.centerFocusedColumn;
    always_center_single_column = cfg.settings.layout.alwaysCenterSingleColumn;
  };

  workspacesSection = dropNulls { count = cfg.settings.workspaces.count; };

  keybindList = map (
    k:
    dropNulls {
      keys = k.keys;
      action = k.action;
      args = if k.args == [ ] then null else k.args;
    }
  ) cfg.settings.keybinds;

  appRuleList = map (
    r:
    dropNulls {
      app = r.app;
      action = r.action;
      column_width = r.columnWidth;
    }
  ) cfg.settings.appRules;

  configValue = lib.filterAttrs (_: v: v != { } && v != [ ]) {
    general = generalSection;
    layout = layoutSection;
    workspaces = workspacesSection;
    keybind = keybindList;
    app_rule = appRuleList;
  };

in
{
  options.services.tiler = {
    enable = mkEnableOption "tiler" // {
      description = "Enable tiler, a scrollable tiling macOS window manager.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.local.tiler;
      defaultText = "pkgs.local.tiler";
      description = "The tiler package to use.";
    };

    settings = mkOption {
      type = settingsSubmodule;
      default = { };
      description = "tiler config.toml contents. Omitted fields fall back to tiler's built-in defaults.";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."tiler/config.toml".source = tomlFormat.generate "tiler-config" configValue;

    launchd.agents.tiler = {
      enable = true;
      config = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        ProgramArguments = [ "${cfg.package}/Applications/TilerApp.app/Contents/MacOS/TilerApp" ];
        StandardOutPath = "/tmp/tiler.log";
        StandardErrorPath = "/tmp/tiler.log";
      };
    };
  };
}
