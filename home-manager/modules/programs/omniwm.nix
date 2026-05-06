{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types;
  cfg = config.programs.omniwm;

  tomlFormat = pkgs.formats.toml { };

  # Convert #rrggbb to OmniWM float RGB
  hexToFloat =
    hex:
    let
      hexToInt =
        h:
        let
          chars = {
            "0" = 0;
            "1" = 1;
            "2" = 2;
            "3" = 3;
            "4" = 4;
            "5" = 5;
            "6" = 6;
            "7" = 7;
            "8" = 8;
            "9" = 9;
            "a" = 10;
            "b" = 11;
            "c" = 12;
            "d" = 13;
            "e" = 14;
            "f" = 15;
            "A" = 10;
            "B" = 11;
            "C" = 12;
            "D" = 13;
            "E" = 14;
            "F" = 15;
          };
        in
        (chars.${builtins.substring 0 1 h} * 16) + chars.${builtins.substring 1 2 h};
      len = builtins.stringLength hex;
      hasAlpha = len == 9;
    in
    {
      red = hexToInt (builtins.substring 1 2 hex) / 255.0;
      green = hexToInt (builtins.substring 3 2 hex) / 255.0;
      blue = hexToInt (builtins.substring 5 2 hex) / 255.0;
      alpha = if hasAlpha then hexToInt (builtins.substring 7 2 hex) / 255.0 else 1.0;
    };

  hexColor = types.strMatching "#[0-9a-fA-F]{6,8}";

  # Generate a stable UUID from a workspace name
  workspaceId =
    name:
    let
      hash = builtins.hashString "sha256" "omniwm-workspace-${name}";
      h = builtins.substring 0 32 hash;
    in
    "${builtins.substring 0 8 h}-${builtins.substring 8 4 h}-${builtins.substring 12 4 h}-${builtins.substring 16 4 h}-${builtins.substring 20 12 h}";

  # Shorthand for nested option declarations (default = { } so children use their own defaults)
  mkSub =
    opts:
    mkOption {
      type = types.submodule { options = opts; };
      default = { };
    };
in
{
  options.programs.omniwm = {
    enable = lib.mkEnableOption "OmniWM tiling window manager";

    package = lib.mkPackageOption pkgs "omniwm" { nullable = true; };

    appearance = mkSub {
      mode = mkOption {
        type = types.enum [
          "dark"
          "light"
          "system"
        ];
        default = "dark";
        description = "Appearance mode for OmniWM.";
      };
    };

    borders = mkSub {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to show window borders.";
      };
      width = mkOption {
        type = types.numbers.positive;
        default = 5.0;
        description = "Width of window borders in points.";
      };
      color = mkOption {
        type = hexColor;
        default = "#2fafff";
        description = "Border color for the focused window (hex).";
      };
    };

    dwindle = mkSub {
      smartSplit = mkOption {
        type = types.bool;
        default = false;
        description = "Enable smart split for dwindle layout.";
      };
      defaultSplitRatio = mkOption {
        type = types.numbers.between 0.0 1.0;
        default = 1.0;
        description = "Default split ratio for dwindle layout.";
      };
      splitWidthMultiplier = mkOption {
        type = types.numbers.positive;
        default = 1.0;
        description = "Split width multiplier for dwindle layout.";
      };
      singleWindowAspectRatio = mkOption {
        type = types.str;
        default = "4:3";
        description = "Aspect ratio for single window in dwindle layout.";
      };
      useGlobalGaps = mkOption {
        type = types.bool;
        default = true;
        description = "Use global gap settings for dwindle layout.";
      };
      moveToRootStable = mkOption {
        type = types.bool;
        default = true;
        description = "Stable move-to-root behavior in dwindle layout.";
      };
    };

    focus = mkSub {
      followsMouse = mkOption {
        type = types.bool;
        default = false;
        description = "Whether focus follows mouse.";
      };
      followsWindowToMonitor = mkOption {
        type = types.bool;
        default = false;
        description = "Whether focus follows window across monitors.";
      };
      moveMouseToFocusedWindow = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to move mouse to focused window.";
      };
    };

    gaps = mkSub {
      size = mkOption {
        type = types.numbers.nonnegative;
        default = 8.0;
        description = "Inner gap size in points.";
      };
      outer = mkSub {
        left = mkOption {
          type = types.numbers.nonnegative;
          default = 8.0;
          description = "Outer gap on the left.";
        };
        right = mkOption {
          type = types.numbers.nonnegative;
          default = 8.0;
          description = "Outer gap on the right.";
        };
        top = mkOption {
          type = types.numbers.nonnegative;
          default = 8.0;
          description = "Outer gap on the top.";
        };
        bottom = mkOption {
          type = types.numbers.nonnegative;
          default = 8.0;
          description = "Outer gap on the bottom.";
        };
      };
    };

    general = mkSub {
      animationsEnabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether animations are enabled.";
      };
      defaultLayoutType = mkOption {
        type = types.enum [
          "niri"
          "dwindle"
        ];
        default = "niri";
        description = "Default layout type.";
      };
      hotkeysEnabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether hotkeys are enabled.";
      };
      ipcEnabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether IPC is enabled.";
      };
      preventSleepEnabled = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to prevent display sleep.";
      };
      updateChecksEnabled = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to check for updates.";
      };
    };

    gestures = mkSub {
      scrollEnabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether trackpad scroll gestures are enabled.";
      };
      scrollModifierKey = mkOption {
        type = types.str;
        default = "optionShift";
        description = "Modifier key for scroll gestures.";
      };
      scrollSensitivity = mkOption {
        type = types.numbers.positive;
        default = 5.0;
        description = "Scroll gesture sensitivity.";
      };
      fingerCount = mkOption {
        type = types.ints.between 2 4;
        default = 3;
        description = "Number of fingers for trackpad gestures.";
      };
      invertDirection = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to invert gesture direction.";
      };
    };

    mouseWarp = mkSub {
      axis = mkOption {
        type = types.enum [
          "horizontal"
          "vertical"
          "both"
          "none"
        ];
        default = "horizontal";
        description = "Mouse warp axis.";
      };
      margin = mkOption {
        type = types.numbers.nonnegative;
        default = 1;
        description = "Mouse warp margin.";
      };
    };

    niri = mkSub {
      alwaysCenterSingleColumn = mkOption {
        type = types.bool;
        default = true;
        description = "Always center a single column.";
      };
      centerFocusedColumn = mkOption {
        type = types.enum [
          "never"
          "onLeftEdge"
          "always"
        ];
        default = "never";
        description = "When to center the focused column.";
      };
      columnWidthPresets = mkOption {
        type = types.listOf types.numbers.positive;
        default = [
          0.3333333333333333
          0.5
          0.66
        ];
        description = "Column width presets for cycling.";
      };
      infiniteLoop = mkOption {
        type = types.bool;
        default = false;
        description = "Whether niri columns loop infinitely.";
      };
      maxVisibleColumns = mkOption {
        type = types.ints.positive;
        default = 2;
        description = "Maximum visible columns in niri layout.";
      };
      maxWindowsPerColumn = mkOption {
        type = types.ints.positive;
        default = 3;
        description = "Maximum windows per column in niri layout.";
      };
      singleWindowAspectRatio = mkOption {
        type = types.str;
        default = "4:3";
        description = "Aspect ratio for single window in niri layout.";
      };
    };

    quakeTerminal = mkSub {
      enabled = mkOption {
        type = types.bool;
        default = false;
        description = "Whether the quake terminal is enabled.";
      };
      position = mkOption {
        type = types.str;
        default = "center";
        description = "Quake terminal position.";
      };
      widthPercent = mkOption {
        type = types.numbers.between 0.0 100.0;
        default = 50.0;
        description = "Quake terminal width as percent.";
      };
      heightPercent = mkOption {
        type = types.numbers.between 0.0 100.0;
        default = 50.0;
        description = "Quake terminal height as percent.";
      };
      animationDuration = mkOption {
        type = types.numbers.nonnegative;
        default = 0.2;
        description = "Quake terminal animation duration.";
      };
      autoHide = mkOption {
        type = types.bool;
        default = false;
        description = "Auto-hide quake terminal on focus loss.";
      };
      opacity = mkOption {
        type = types.numbers.between 0.0 1.0;
        default = 1.0;
        description = "Quake terminal opacity.";
      };
      monitorMode = mkOption {
        type = types.str;
        default = "focusedWindow";
        description = "Quake terminal monitor mode.";
      };
      useCustomFrame = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use a custom frame for the quake terminal.";
      };
    };

    statusBar = mkSub {
      showAppNames = mkOption {
        type = types.bool;
        default = false;
        description = "Show app names in status bar.";
      };
      showWorkspaceName = mkOption {
        type = types.bool;
        default = false;
        description = "Show workspace name in status bar.";
      };
      useWorkspaceId = mkOption {
        type = types.bool;
        default = false;
        description = "Use workspace ID in status bar.";
      };
    };

    workspaceBar = mkSub {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether the workspace bar is enabled.";
      };
      showLabels = mkOption {
        type = types.bool;
        default = true;
        description = "Show labels in workspace bar.";
      };
      showFloatingWindows = mkOption {
        type = types.bool;
        default = false;
        description = "Show floating windows in workspace bar.";
      };
      windowLevel = mkOption {
        type = types.str;
        default = "popup";
        description = "Window level for workspace bar.";
      };
      position = mkOption {
        type = types.str;
        default = "overlappingMenuBar";
        description = "Position of workspace bar.";
      };
      notchAware = mkOption {
        type = types.bool;
        default = true;
        description = "Whether workspace bar is notch-aware.";
      };
      deduplicateAppIcons = mkOption {
        type = types.bool;
        default = false;
        description = "Deduplicate app icons in workspace bar.";
      };
      hideEmptyWorkspaces = mkOption {
        type = types.bool;
        default = false;
        description = "Hide empty workspaces in workspace bar.";
      };
      reserveLayoutSpace = mkOption {
        type = types.bool;
        default = false;
        description = "Reserve layout space for workspace bar.";
      };
      height = mkOption {
        type = types.numbers.positive;
        default = 24.0;
        description = "Height of workspace bar.";
      };
      backgroundOpacity = mkOption {
        type = types.numbers.between 0.0 1.0;
        default = 0.1;
        description = "Background opacity of workspace bar.";
      };
      xOffset = mkOption {
        type = types.numbers.nonnegative;
        default = 0.0;
        description = "X offset of workspace bar.";
      };
      yOffset = mkOption {
        type = types.numbers.nonnegative;
        default = 0.0;
        description = "Y offset of workspace bar.";
      };
      labelFontSize = mkOption {
        type = types.numbers.between 10.0 16.0;
        default = 12.0;
        description = "Label font size in workspace bar.";
      };
      accentColor = mkOption {
        type = hexColor;
        default = "#ffffff";
        description = "Accent color for workspace bar (hex).";
      };
      textColor = mkOption {
        type = hexColor;
        default = "#ffffff";
        description = "Text color for workspace bar (hex).";
      };
    };

    keybindings = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Keybindings mapping OmniWM command IDs to key combinations.
        See https://github.com/BarutSRB/OmniWM for available command IDs.
      '';
      example = lib.literalExpression ''
        {
          "focus.left" = "Command+Control+H";
          "focus.down" = "Command+Control+J";
          "focus.up" = "Command+Control+K";
          "focus.right" = "Command+Control+L";
        }
      '';
    };

    appRules = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            bundleId = mkOption {
              type = types.str;
              description = "macOS bundle identifier.";
            };
            id = mkOption {
              type = types.str;
              description = "Unique rule identifier (UUID).";
            };
            minHeight = mkOption {
              type = types.numbers.positive;
              description = "Minimum window height.";
            };
            minWidth = mkOption {
              type = types.numbers.positive;
              description = "Minimum window width.";
            };
          };
        }
      );
      default = [ ];
      description = "Application window rules for OmniWM.";
    };

    workspaces = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "Workspace name/number.";
            };
            displayName = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Display name (emoji, etc.).";
            };
            layoutType = mkOption {
              type = types.str;
              default = "niri";
              description = "Default layout type.";
            };
            monitor = mkOption {
              type = types.enum [
                "main"
                "secondary"
              ];
              default = "main";
              description = "Monitor assignment.";
            };
          };
        }
      );
      default = [
        { name = "1"; }
        { name = "2"; }
        { name = "3"; }
        { name = "4"; }
      ];
      description = "Workspace configurations. IDs are auto-generated from names.";
    };

    launchd = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Configure the launchd agent to manage the OmniWM process.

          The first time this is enabled, macOS will prompt you to allow this background
          item in System Settings.

          You can verify the service is running correctly from your terminal.
          Run: `launchctl list | grep omniwm`

          In case of failure, check the logs with `cat /tmp/omniwm.err.log`.

          For more detailed service status, run
          `launchctl print gui/$(id -u)/org.nix-community.home.omniwm`.
        '';
      };
      keepAlive = mkOption {
        type = types.bool;
        default = true;
        description = "Whether the launchd service should be kept alive.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "programs.omniwm" pkgs lib.platforms.darwin) ];

    home = {
      packages = lib.mkIf (cfg.package != null) [ cfg.package ];

      file.".config/omniwm/settings.toml".source = tomlFormat.generate "omniwm-settings" {
        version = 5;
        monitorBarOverrides = [ ];
        monitorDwindleOverrides = [ ];
        monitorNiriOverrides = [ ];
        monitorOrientationOverrides = [ ];

        inherit (cfg)
          appearance
          dwindle
          focus
          gaps
          general
          ;
        inherit (cfg)
          gestures
          niri
          quakeTerminal
          statusBar
          ;

        borders = cfg.borders // {
          color = hexToFloat cfg.borders.color;
        };

        mouseWarp = cfg.mouseWarp // {
          monitorOrder = [ ];
        };

        workspaceBar = cfg.workspaceBar // {
          accentColor = hexToFloat cfg.workspaceBar.accentColor;
          textColor = hexToFloat cfg.workspaceBar.textColor;
        };

        state = {
          commandPaletteLastMode = "windows";
          hiddenBarIsCollapsed = true;
        };

        hotkeys = lib.mapAttrsToList (id: binding: { inherit id binding; }) cfg.keybindings;

        appRules = map (rule: {
          inherit (rule) bundleId id;
          minHeight = rule.minHeight;
          minWidth = rule.minWidth;
        }) cfg.appRules;

        workspaces = map (
          ws:
          {
            id = workspaceId ws.name;
            name = ws.name;
            layoutType = ws.layoutType;
            monitorAssignment.type = ws.monitor;
          }
          // lib.optionalAttrs (ws.displayName != null) { inherit (ws) displayName; }
        ) cfg.workspaces;
      };

      # OmniWM loves writing to its own config. Please don't.
      file.".config/omniwm/settings.toml".force = true;
    };

    launchd.agents.omniwm = {
      enable = cfg.launchd.enable;
      config = {
        Program = "${cfg.package}/Applications/OmniWM.app/Contents/MacOS/OmniWM";
        KeepAlive = cfg.launchd.keepAlive;
        RunAtLoad = true;
        StandardOutPath = "/tmp/omniwm.log";
        StandardErrorPath = "/tmp/omniwm.err.log";
      };
    };
  };
}
