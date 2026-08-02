{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption concatStringsSep;
  cfg = config.services.localterm;
  fontcfg = config.home.fonts.terminal;

  # The daemon is launched in the foreground so launchd/systemd supervise it.
  startArgs = [
    "start"
    "--foreground"
    "--port"
    (toString cfg.port)
    "--host"
    cfg.host
  ];
in
{
  options.services.localterm = {
    enable = mkEnableOption "localterm terminal daemon";

    package = mkOption {
      type = types.package;
      default = pkgs.localterm;
      description = "The localterm package to use.";
    };

    port = mkOption {
      type = types.port;
      default = 3417;
      description = "Host port the localterm daemon listens on.";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host address the localterm daemon binds to.";
    };

    fullPath = mkOption {
      type = types.bool;
      default = true;
      description = "Set LOCALTERM_PTY_FULL_PATH=1 so PTY shells inherit the daemon's full PATH.";
    };

    zdotdir = mkOption {
      type = types.nullOr types.path;
      default = "${config.xdg.configHome}/zsh";
      defaultText = "config.xdg.configHome + \"/zsh\"";
      description = "ZDOTDIR passed to the daemon so the zsh shell hook sources the real rc files. localterm computes the hook's rc dir from the daemon's env (__LOCALTERM_ORIG_ZDOTDIR or ZDOTDIR), so an unset ZDOTDIR makes it source ~/.zshrc instead of ~/.config/zsh/.zshrc.";
    };

    font = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Write a declarative ~/.localterm/fonts.json from home.fonts.terminal.";
      };

      family = mkOption {
        type = types.str;
        default = fontcfg.monospace;
        defaultText = "config.home.fonts.terminal.monospace";
        description = "Custom terminal font family for localterm.";
      };

      nerdFont = mkOption {
        type = types.bool;
        default = false;
        description = "Enable Nerd Font icons by appending a symbols-only Nerd Font to the font stack.";
      };

      ligatures = mkOption {
        type = types.bool;
        default = false;
        description = "Enable font ligatures.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    # FontStore silently resets to defaults if the file fails the v1 schema.
    home.file.".localterm/fonts.json" = lib.mkIf cfg.font.enable {
      text = builtins.toJSON {
        version = 1;
        activeFontId = "custom";
        customFontFamily = cfg.font.family;
        nerdFontEnabled = cfg.font.nerdFont;
        ligaturesEnabled = cfg.font.ligatures;
      };
    };

    systemd.user.services.localterm = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "localterm terminal daemon";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = concatStringsSep " " ([ (lib.getExe cfg.package) ] ++ startArgs);
        Restart = "on-failure";
        RestartSec = 5;
        Slice = "app.slice";
        Environment =
          (lib.mkIf cfg.fullPath [ "LOCALTERM_PTY_FULL_PATH=1" ])
          ++ (lib.mkIf (cfg.zdotdir != null) [ "ZDOTDIR=${cfg.zdotdir}" ]);
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    launchd.agents.localterm = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        ProgramArguments = [ (lib.getExe cfg.package) ] ++ startArgs;
        EnvironmentVariables =
          (lib.mkIf cfg.fullPath {
            LOCALTERM_PTY_FULL_PATH = "1";
          })
          // (lib.mkIf (cfg.zdotdir != null) {
            ZDOTDIR = "${cfg.zdotdir}";
          });
        StandardOutPath = "/tmp/localterm.log";
        StandardErrorPath = "/tmp/localterm.log";
      };
    };
  };
}
