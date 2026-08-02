{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkOption
    types
    mkEnableOption
    concatStringsSep
    ;
  cfg = config.services.localterm;
  fontcfg = config.home.fonts.terminal;

  startArgs = [
    "start"
    "--foreground"
    "--port"
    (toString cfg.port)
    "--host"
    cfg.host
  ];

  # We can't use home.file here because it symlinks to the read-only store,
  # and the daemon persists cdpPort/graceSeconds changes back into this file.
  baseConfig = pkgs.writeText "localterm-config.json" (
    builtins.toJSON {
      version = 1;
      cdpPort = null;
      identity = {
        provider = cfg.identity.provider;
      }
      // lib.optionalAttrs (cfg.identity.provider == "oidc") {
        issuer = cfg.identity.oidc.issuer;
        clientId = cfg.identity.oidc.clientId;
        claim = cfg.identity.oidc.claim;
        scope = cfg.identity.oidc.scope;
      };
    }
  );

  writeConfigScript =
    if cfg.identity.oidc.clientSecretFile != null then
      pkgs.writeShellScript "localterm-write-config" ''
        mkdir -p "$HOME/.localterm"
        ${lib.getExe' pkgs.jq "jq"} \
          --arg secret "$(cat ${lib.escapeShellArg cfg.identity.oidc.clientSecretFile})" \
          '.identity.clientSecret = $secret' \
          ${baseConfig} > "$HOME/.localterm/config.json"
      ''
    else
      pkgs.writeShellScript "localterm-write-config" ''
        mkdir -p "$HOME/.localterm"
        cp ${baseConfig} "$HOME/.localterm/config.json"
      '';
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

    publicUrl = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Announced public origin (e.g. `https://term.example.com:3417`). Drives
        the OIDC redirect URI and the network-policy host allowlist so a
        DNS-named reverse proxy fronting a non-loopback bind is accepted.
        Null lets the CLI auto-resolve (loopback / portless / tailnet).
        Requires the localterm package patch that reads LOCALTERM_PUBLIC_URL.
      '';
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

    identity = {
      provider = mkOption {
        type = types.enum [
          "none"
          "header"
          "passkey"
          "oidc"
        ];
        default = "none";
        description = ''
          Identity provider for multi-user access. See
          https://github.com/monotykamary/localterm/blob/main/docs/identity.md.
          "none" is single-authority mode (no login).
        '';
      };

      oidc = {
        issuer = mkOption {
          type = types.str;
          description = "OIDC issuer URL (e.g. https://keycloak.example.com/realms/myrealm).";
        };

        clientId = mkOption {
          type = types.str;
          description = "OIDC client ID registered with the IdP.";
        };

        clientSecretFile = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Path to a file containing the OIDC client secret (e.g. a sops secret path).
            Null for a public (PKCE-only) client — the config file is then fully
            declarative with no ExecStartPre secret injection.
          '';
        };

        claim = mkOption {
          type = types.str;
          default = "email";
          description = "UserInfo claim to use as the identity (falls back to sub when absent).";
        };

        scope = mkOption {
          type = types.str;
          default = "openid email";
          description = "Space-separated OIDC scopes.";
        };
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
          (lib.optional cfg.fullPath "LOCALTERM_PTY_FULL_PATH=1")
          ++ (lib.optional (cfg.zdotdir != null) "ZDOTDIR=${cfg.zdotdir}")
          ++ (lib.optional (cfg.publicUrl != null) "LOCALTERM_PUBLIC_URL=${cfg.publicUrl}");
      }
      // lib.optionalAttrs (cfg.identity.provider != "none") { ExecStartPre = [ writeConfigScript ]; };

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
          (lib.optionalAttrs cfg.fullPath { LOCALTERM_PTY_FULL_PATH = "1"; })
          // (lib.optionalAttrs (cfg.zdotdir != null) { ZDOTDIR = "${cfg.zdotdir}"; })
          // (lib.optionalAttrs (cfg.publicUrl != null) { LOCALTERM_PUBLIC_URL = cfg.publicUrl; });
        StandardOutPath = "/tmp/localterm.log";
        StandardErrorPath = "/tmp/localterm.log";
      };
    };
  };
}
