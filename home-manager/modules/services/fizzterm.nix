{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.fizzterm;

  tomlFormat = pkgs.formats.toml { };

  paletteSubmodule = types.submodule {
    options = {
      foreground = mkOption {
        type = types.str;
        description = "Terminal foreground color.";
      };
      background = mkOption {
        type = types.str;
        description = "Terminal background color.";
      };
      cursor = mkOption {
        type = types.str;
        description = "Cursor color.";
      };
      selection_background = mkOption {
        type = types.str;
        description = "Selection background color.";
      };
      black = mkOption {
        type = types.str;
        description = "ANSI black.";
      };
      red = mkOption {
        type = types.str;
        description = "ANSI red.";
      };
      green = mkOption {
        type = types.str;
        description = "ANSI green.";
      };
      yellow = mkOption {
        type = types.str;
        description = "ANSI yellow.";
      };
      blue = mkOption {
        type = types.str;
        description = "ANSI blue.";
      };
      magenta = mkOption {
        type = types.str;
        description = "ANSI magenta.";
      };
      cyan = mkOption {
        type = types.str;
        description = "ANSI cyan.";
      };
      white = mkOption {
        type = types.str;
        description = "ANSI white.";
      };
      bright_black = mkOption {
        type = types.str;
        description = "ANSI bright black.";
      };
      bright_red = mkOption {
        type = types.str;
        description = "ANSI bright red.";
      };
      bright_green = mkOption {
        type = types.str;
        description = "ANSI bright green.";
      };
      bright_yellow = mkOption {
        type = types.str;
        description = "ANSI bright yellow.";
      };
      bright_blue = mkOption {
        type = types.str;
        description = "ANSI bright blue.";
      };
      bright_magenta = mkOption {
        type = types.str;
        description = "ANSI bright magenta.";
      };
      bright_cyan = mkOption {
        type = types.str;
        description = "ANSI bright cyan.";
      };
      bright_white = mkOption {
        type = types.str;
        description = "ANSI bright white.";
      };
    };
  };

  settings = lib.filterAttrs (_: v: v != null) {
    server = lib.filterAttrs (_: v: v != null) {
      address = cfg.server.address;
      allowed_user = cfg.server.allowedUser;
      user_header = cfg.server.userHeader;
      websocket_max_lifetime_seconds = cfg.server.websocketMaxLifetimeSeconds;
    };
    session = lib.filterAttrs (_: v: v != null) {
      shell = cfg.session.shell;
      replay_bytes = cfg.session.replayBytes;
    };
    font = lib.filterAttrs (_: v: v != null) {
      family = cfg.font.family;
      size = cfg.font.size;
      directory = cfg.font.directory;
    };
    theme = lib.filterAttrs (_: v: v != null) {
      mode = cfg.theme.mode;
      dark = cfg.theme.dark;
      light = cfg.theme.light;
    };
    clipboard = lib.filterAttrs (_: v: v != null) {
      osc52 = cfg.clipboard.osc52;
      max_bytes = cfg.clipboard.maxBytes;
    };
    notifications = lib.filterAttrs (_: v: v != null) {
      enabled = cfg.notifications.enabled;
      when_focused = cfg.notifications.whenFocused;
    };
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
        type = types.str;
        default = "127.0.0.1:3417";
        description = "Address the fizzterm server binds to.";
      };

      allowedUser = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Require this exact authenticated user value from the proxy.";
      };

      userHeader = mkOption {
        type = types.str;
        default = "x-forwarded-email";
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
        type = types.nullOr types.str;
        default = null;
        description = "Shell to launch in each session. Null uses the system default.";
      };

      replayBytes = mkOption {
        type = types.ints.positive;
        default = 1048576;
        description = "Maximum bytes of session output kept for replay after reconnect.";
      };
    };

    font = {
      family = mkOption {
        type = types.str;
        default = "monospace";
        description = "Terminal font family.";
      };

      size = mkOption {
        type = types.ints.positive;
        default = 14;
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
        type = types.enum [
          "auto"
          "dark"
          "light"
        ];
        default = "auto";
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
        type = types.enum [
          "disabled"
          "allow"
        ];
        default = "allow";
        description = "OSC 52 clipboard mode.";
      };

      maxBytes = mkOption {
        type = types.ints.positive;
        default = 65536;
        description = "Maximum bytes accepted for OSC 52 clipboard writes.";
      };
    };

    notifications = {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = "Enable terminal notifications.";
      };

      whenFocused = mkOption {
        type = types.bool;
        default = false;
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
