{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.coord;

  tomlFormat = pkgs.formats.toml { };

  gridLevelSubmodule = types.submodule {
    options = {
      cols = mkOption {
        type = types.ints.positive;
        description = "Number of columns in this grid level.";
      };

      rows = mkOption {
        type = types.ints.positive;
        description = "Number of rows in this grid level.";
      };

      colKeys = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Keys used to select columns. null means omitted (use row-only selection).";
      };

      rowKeys = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Keys used to select rows. null means omitted (use col-only selection).";
      };

      labelOrder = mkOption {
        type = types.str;
        default = "col_row";
        description = "Label ordering for cells.";
      };
  };
  };

  subgridSubmodule = types.submodule {
    options = {
      cols = mkOption {
        type = types.ints.positive;
        default = 10;
        description = "Number of columns in the subgrid.";
      };

      rows = mkOption {
        type = types.ints.positive;
        default = 3;
        description = "Number of rows in the subgrid.";
      };

      keys = mkOption {
        type = types.str;
        default = "qwertyuiopasdfghjkl;zxcvbnm,./";
        description = "Keys used for subgrid cell selection.";
      };

      nudgeEnabled = mkOption {
        type = types.bool;
        default = false;
        description = "Whether nudge mode is enabled for the subgrid.";
      };

      nudgesPerCell = mkOption {
        type = types.ints.positive;
        default = 4;
        description = "Number of nudge steps per cell.";
      };
  };
  };

  gridSubmodule = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = "Name of the grid.";
      };

      monitorAssignment = mkOption {
        type = types.either types.str (types.attrsOf types.str);
        default = "Auto";
        description = "Monitor assignment for this grid. Either a string or an attribute set mapping monitor names to grid names.";
      };

      levels = mkOption {
        type = types.listOf gridLevelSubmodule;
        default = [ ];
        description = "Grid levels defining column/row layout and key bindings.";
      };

      subgrid = mkOption {
        type = subgridSubmodule;
        default = { };
        description = "Subgrid configuration for fine-grained cell selection.";
      };
    };
  };

  settings = {
    behavior = {
      multi_click_threshold_ms = cfg.behavior.multiClickThresholdMs;
      continuous_overlay = cfg.behavior.continuousOverlay;
      free_mode_auto_off_secs = cfg.behavior.freeModeAutoOffSecs;
      free_mode_smooth = cfg.behavior.freeModeSmooth;
      free_mode_base_speed = cfg.behavior.freeModeBaseSpeed;
      free_mode_max_speed = cfg.behavior.freeModeMaxSpeed;
      free_mode_accel = cfg.behavior.freeModeAccel;
      free_mode_slow_factor = cfg.behavior.freeModeSlowFactor;
      free_mode_fast_factor = cfg.behavior.freeModeFastFactor;
    };
    keymap = {
      show_overlay = cfg.keymap.showOverlay;
      hide_overlay = cfg.keymap.hideOverlay;
      undo_selection = cfg.keymap.undoSelection;
      execute_action = cfg.keymap.executeAction;
      free_mode_key = cfg.keymap.freeModeKey;
      free_mode = {
        up = cfg.keymap.freeMode.up;
        down = cfg.keymap.freeMode.down;
        left = cfg.keymap.freeMode.left;
        right = cfg.keymap.freeMode.right;
        slower = cfg.keymap.freeMode.slower;
        faster = cfg.keymap.freeMode.faster;
        click_left = cfg.keymap.freeMode.clickLeft;
        click_middle = cfg.keymap.freeMode.clickMiddle;
        scroll_up = cfg.keymap.freeMode.scrollUp;
        scroll_down = cfg.keymap.freeMode.scrollDown;
      };
    };
    style = {
      master_opacity = cfg.style.masterOpacity;
      background_color = cfg.style.backgroundColor;
      highlight_color = cfg.style.highlightColor;
      grid_line_color = cfg.style.gridLineColor;
      grid_line_width = cfg.style.gridLineWidth;
      font_family = cfg.style.fontFamily;
      font_size = cfg.style.fontSize;
      font_color = cfg.style.fontColor;
      cursor_color = cfg.style.cursorColor;
      cursor_radius = cfg.style.cursorRadius;
      render_scale = cfg.style.renderScale;
    };
    grids = map (grid: {
      name = grid.name;
      monitor_assignment = grid.monitorAssignment;
      levels = map (level: lib.filterAttrs (_: v: v != null) {
        cols = level.cols;
        rows = level.rows;
        col_keys = level.colKeys;
        row_keys = level.rowKeys;
        label_order = level.labelOrder;
      }) grid.levels;
      subgrid = {
        cols = grid.subgrid.cols;
        rows = grid.subgrid.rows;
        keys = grid.subgrid.keys;
        nudge_enabled = grid.subgrid.nudgeEnabled;
        nudges_per_cell = grid.subgrid.nudgesPerCell;
      };
    }) cfg.grids;
  };

in
{
  options.services.coord = {
    enable = mkEnableOption "coord" // {
      description = "Enable coord, a keyboard-controlled mouse for Wayland.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.local.coord;
      defaultText = "pkgs.local.coord";
      description = "The coord package to use.";
    };

    behavior = {
      multiClickThresholdMs = mkOption {
        type = types.ints.positive;
        default = 250;
        description = "Window (ms) during which a second press produces a double-click.";
      };

      continuousOverlay = mkOption {
        type = types.bool;
        default = false;
        description = "Whether the overlay stays visible continuously.";
      };

      freeModeAutoOffSecs = mkOption {
        type = types.ints.positive;
        default = 30;
        description = "Seconds of inactivity before free mode turns off automatically.";
      };

      freeModeSmooth = mkOption {
        type = types.bool;
        default = true;
        description = "Whether smooth (interpolated) cursor movement is enabled in free mode.";
      };

      freeModeBaseSpeed = mkOption {
        type = types.float;
        default = 150.0;
        description = "Smooth free-mode base velocity (logical px/sec).";
      };

      freeModeMaxSpeed = mkOption {
        type = types.float;
        default = 650.0;
        description = "Smooth free-mode maximum velocity (logical px/sec).";
      };

      freeModeAccel = mkOption {
        type = types.float;
        default = 1000.0;
        description = "Smooth free-mode acceleration (logical px/sec²).";
      };

      freeModeSlowFactor = mkOption {
        type = types.float;
        default = 0.3;
        description = "Factor applied to base speed when the slow key is held.";
      };

      freeModeFastFactor = mkOption {
        type = types.float;
        default = 2.5;
        description = "Factor applied to base speed when the fast key is held.";
      };
    };

    keymap = {
      showOverlay = mkOption {
        type = types.str;
        default = "meta_l";
        description = "Key that shows the overlay.";
      };

      hideOverlay = mkOption {
        type = types.str;
        default = "escape";
        description = "Key that hides the overlay.";
      };

      undoSelection = mkOption {
        type = types.str;
        default = "backspace";
        description = "Key that undoes the current selection.";
      };

      executeAction = mkOption {
        type = types.str;
        default = "space";
        description = "Key that executes the current action.";
      };

      freeModeKey = mkOption {
        type = types.str;
        default = "alt_l";
        description = "Key that activates free mode (combined with the overlay trigger).";
      };

      freeMode = {
        up = mkOption {
          type = types.str;
          default = "k";
          description = "Key for moving the cursor up in free mode.";
        };

        down = mkOption {
          type = types.str;
          default = "j";
          description = "Key for moving the cursor down in free mode.";
        };

        left = mkOption {
          type = types.str;
          default = "h";
          description = "Key for moving the cursor left in free mode.";
        };

        right = mkOption {
          type = types.str;
          default = "l";
          description = "Key for moving the cursor right in free mode.";
        };

        slower = mkOption {
          type = types.str;
          default = "a";
          description = "Key for decreasing cursor speed in free mode.";
        };

        faster = mkOption {
          type = types.str;
          default = "s";
          description = "Key for increasing cursor speed in free mode.";
        };

        clickLeft = mkOption {
          type = types.str;
          default = "space";
          description = "Key for left-click in free mode.";
        };

        clickMiddle = mkOption {
          type = types.str;
          default = "e";
          description = "Key for middle-click in free mode.";
        };

        scrollUp = mkOption {
          type = types.str;
          default = "m";
          description = "Key for scrolling up in free mode.";
        };

        scrollDown = mkOption {
          type = types.str;
          default = ",";
          description = "Key for scrolling down in free mode.";
        };
      };
    };

    style = {
      masterOpacity = mkOption {
        type = types.float;
        default = 0.85;
        description = "Master opacity for the overlay.";
      };

      backgroundColor = mkOption {
        type = types.str;
        default = "#00000080";
        description = "Background color of the overlay.";
      };

      highlightColor = mkOption {
        type = types.str;
        default = "#4080ff";
        description = "Highlight color for selected cells.";
      };

      gridLineColor = mkOption {
        type = types.str;
        default = "#ffffff59";
        description = "Color of grid lines.";
      };

      gridLineWidth = mkOption {
        type = types.float;
        default = 1.0;
        description = "Width of grid lines.";
      };

      fontFamily = mkOption {
        type = types.str;
        default = "monospace";
        description = "Font family for labels.";
      };

      fontSize = mkOption {
        type = types.float;
        default = 14.0;
        description = "Font size for labels.";
      };

      fontColor = mkOption {
        type = types.str;
        default = "#ffffff";
        description = "Font color for labels.";
      };

      cursorColor = mkOption {
        type = types.str;
        default = "#40ff80";
        description = "Color of the cursor indicator.";
      };

      cursorRadius = mkOption {
        type = types.float;
        default = 4.0;
        description = "Radius of the cursor indicator.";
      };

      renderScale = mkOption {
        type = types.either (types.enum [ "auto" ]) types.ints.positive;
        default = "auto";
        description = ''Render scale: "auto" uses the monitor scale, or a positive integer for explicit logical-res rendering.'';
      };
    };

    grids = mkOption {
      type = types.listOf gridSubmodule;
      default = [
        {
          name = "default";
          monitorAssignment = "Auto";
          levels = [
            {
              cols = 10;
              rows = 30;
              colKeys = "asdfghjkl;";
              rowKeys = "qwertyuiopasdfghjkl;zxcvbnm,./";
              labelOrder = "col_row";
            }
          ];
        }
      ];
      description = "Grid definitions for the overlay.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."coord/config.toml" = {
      source = tomlFormat.generate "coord-config" settings;
    };

    systemd.user.services.coord =
      lib.mkIf pkgs.stdenv.isLinux
        {
          Unit = {
            Description = "coord - keyboard-controlled mouse for Wayland";
            PartOf = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = lib.getExe cfg.package;
            Restart = "on-failure";
            RestartSec = 5;
            Slice = "app.slice";
          };

          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
        };

    launchd.agents.coord =
      lib.mkIf pkgs.stdenv.isDarwin
        {
          enable = true;
          config = {
            RunAtLoad = true;
            KeepAlive = true;
            ProcessType = "Interactive";
            ProgramArguments = [ (lib.getExe cfg.package) ];
            StandardOutPath = "/tmp/coord.log";
            StandardErrorPath = "/tmp/coord.log";
          };
        };
  };
}
