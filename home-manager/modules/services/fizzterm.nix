{
  config,
  lib,
  options,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.fizzterm;
  opt = options.services.fizzterm;

  tomlFormat = pkgs.formats.toml { };

  mkPaletteOption =
    description:
    mkOption {
      type = types.nullOr types.str;
      default = null;
      inherit description;
    };

  paletteSubmodule = types.submodule {
    options = {
      foreground = mkPaletteOption "Terminal foreground color.";
      background = mkPaletteOption "Terminal background color.";
      black = mkPaletteOption "ANSI black.";
      red = mkPaletteOption "ANSI red.";
      green = mkPaletteOption "ANSI green.";
      yellow = mkPaletteOption "ANSI yellow.";
      blue = mkPaletteOption "ANSI blue.";
      magenta = mkPaletteOption "ANSI magenta.";
      cyan = mkPaletteOption "ANSI cyan.";
      white = mkPaletteOption "ANSI white.";
      bright_black = mkPaletteOption "ANSI bright black.";
      bright_red = mkPaletteOption "ANSI bright red.";
      bright_green = mkPaletteOption "ANSI bright green.";
      bright_yellow = mkPaletteOption "ANSI bright yellow.";
      bright_blue = mkPaletteOption "ANSI bright blue.";
      bright_magenta = mkPaletteOption "ANSI bright magenta.";
      bright_cyan = mkPaletteOption "ANSI bright cyan.";
      bright_white = mkPaletteOption "ANSI bright white.";
    };
  };

  # Only include options that were explicitly set so unset options fall back
  # to fizzterm's built-in defaults instead of being written to config.toml.
  onlyIfDefined =
    option: key: value:
    if option.isDefined && value != null && value != { } && value != [ ] then
      builtins.listToAttrs [ (lib.nameValuePair key value) ]
    else
      { };

  settings = lib.filterAttrs (_: v: v != { }) {
    server =
      onlyIfDefined opt.server.address "address" cfg.server.address
      // onlyIfDefined opt.server.allowedUser "allowed_user" cfg.server.allowedUser
      // onlyIfDefined opt.server.userHeader "user_header" cfg.server.userHeader
      //
        onlyIfDefined opt.server.websocketMaxLifetimeSeconds "websocket_max_lifetime_seconds"
          cfg.server.websocketMaxLifetimeSeconds;
    session =
      onlyIfDefined opt.session.shell "shell" cfg.session.shell
      // onlyIfDefined opt.session.env "env" cfg.session.env
      // onlyIfDefined opt.session.replayBytes "replay_bytes" cfg.session.replayBytes;
    font =
      onlyIfDefined opt.font.family "family" cfg.font.family
      // onlyIfDefined opt.font.size "size" cfg.font.size
      // onlyIfDefined opt.font.directory "directory" cfg.font.directory;
    theme =
      onlyIfDefined opt.theme.mode "mode" cfg.theme.mode
      // onlyIfDefined opt.theme.dark "dark" (lib.filterAttrs (_: v: v != null) cfg.theme.dark)
      // onlyIfDefined opt.theme.light "light" (lib.filterAttrs (_: v: v != null) cfg.theme.light);
    clipboard =
      onlyIfDefined opt.clipboard.osc52 "osc52" cfg.clipboard.osc52
      // onlyIfDefined opt.clipboard.maxBytes "max_bytes" cfg.clipboard.maxBytes;
    notifications =
      onlyIfDefined opt.notifications.enabled "enabled" cfg.notifications.enabled
      // onlyIfDefined opt.notifications.whenFocused "when_focused" cfg.notifications.whenFocused;
  };
in
{
  options.services.fizzterm = {
    enable = mkEnableOption "fizzterm terminal daemon";

    package = mkOption {
      type = types.package;
      default = pkgs.local.fizzterm;
      description = "The fizzterm package to use.";
    };

    server = {
      address = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Address the fizzterm server binds to.";
      };

      allowedUser = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Require this exact authenticated user value from the proxy.";
      };

      userHeader = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "HTTP header carrying the authenticated user identity.";
      };

      websocketMaxLifetimeSeconds = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Close WebSockets after this many seconds to force re-authentication.";
      };
    };

    session = {
      shell = mkOption {
        type = types.nullOr (types.listOf types.str);
        default = null;
        description = "Command line used to launch the shell in each session. Null uses the system default.";
      };

      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Environment variables set for each session.";
      };

      replayBytes = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Maximum bytes of session output kept for replay after reconnect.";
      };
    };

    font = {
      family = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Terminal font family.";
      };

      size = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Terminal font size.";
      };

      directory = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Directory to scan for web fonts served to the browser.";
      };
    };

    theme = {
      mode = mkOption {
        type = types.nullOr (
          types.enum [
            "auto"
            "dark"
            "light"
          ]
        );
        default = null;
        description = "Default theme palette mode.";
      };

      dark = mkOption {
        type = paletteSubmodule;
        default = { };
        description = "Dark palette colors.";
      };

      light = mkOption {
        type = paletteSubmodule;
        default = { };
        description = "Light palette colors.";
      };
    };

    clipboard = {
      osc52 = mkOption {
        type = types.nullOr (
          types.enum [
            "disabled"
            "allow"
          ]
        );
        default = null;
        description = "OSC 52 clipboard mode.";
      };

      maxBytes = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Maximum bytes accepted for OSC 52 clipboard writes.";
      };
    };

    notifications = {
      enabled = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "Enable terminal notifications.";
      };

      whenFocused = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "Show notifications when the tab is focused.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."fizzterm/config.toml" = {
      source = tomlFormat.generate "fizzterm-config" settings;
    };

    systemd.user.services.fizzterm = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "fizzterm terminal daemon";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
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

    launchd.agents.fizzterm = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        ProgramArguments = [ (lib.getExe cfg.package) ];
        StandardOutPath = "/tmp/fizzterm.log";
        StandardErrorPath = "/tmp/fizzterm.log";
      };
    };
  };
}
