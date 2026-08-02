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

  startArgs = [
    "start"
    "--foreground"
    "--port"
    (toString cfg.port)
    "--host"
    cfg.host
  ];

  identityConfig = {
    provider = cfg.identity.provider;
  }
  // lib.optionalAttrs (cfg.identity.provider == "oidc") {
    issuer = cfg.identity.oidc.issuer;
    clientId = cfg.identity.oidc.clientId;
    claim = cfg.identity.oidc.claim;
    scope = cfg.identity.oidc.scope;
  }
  // lib.optionalAttrs (cfg.identity.provider == "header") {
    trustedProxy = cfg.identity.header.trustedProxy;
  }
  // lib.optionalAttrs (cfg.identity.header.headerName != null) {
    header = cfg.identity.header.headerName;
  };
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
        Announced public origin (e.g. `https://term.example.com`). Drives
        the network-policy host allowlist so a DNS-named reverse proxy
        fronting a non-loopback bind is accepted. Null lets the CLI
        auto-resolve (loopback / portless / tailnet).
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
        default = "monospace";
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

      header = {
        headerName = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Header name to read the user identity from (defaults to X-Forwarded-User).";
        };

        trustedProxy = mkOption {
          type = types.str;
          default = "loopback";
          description = ''
            CIDR or shorthand (loopback/private) the trusted proxy connects
            from. The identity header is only honored for requests from this
            range.
          '';
        };
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

    # `force` overwrites the daemon's runtime-persisted cdpPort/graceSeconds
    # on each activation; those are non-critical defaults the daemon re-derives.
    home.file.".localterm/config.json" = lib.mkIf (cfg.identity.provider != "none") {
      force = true;
      text = builtins.toJSON {
        version = 1;
        cdpPort = null;
        identity = identityConfig;
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
          (lib.optionalAttrs cfg.fullPath { LOCALTERM_PTY_FULL_PATH = "1"; })
          // (lib.optionalAttrs (cfg.zdotdir != null) { ZDOTDIR = "${cfg.zdotdir}"; })
          // (lib.optionalAttrs (cfg.publicUrl != null) { LOCALTERM_PUBLIC_URL = cfg.publicUrl; });
        StandardOutPath = "/tmp/localterm.log";
        StandardErrorPath = "/tmp/localterm.log";
      };
    };
  };
}
