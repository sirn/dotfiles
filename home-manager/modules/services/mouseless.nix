{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.mouseless;
in
{
  options.services.mouseless = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Mouseless for keyboard-driven mouse control.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.local.mouseless;
      defaultText = "pkgs.local.mouseless";
      description = "The Mouseless package to use.";
    };

    launchd = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Configure the launchd agent to manage the Mouseless process.

          The first time this is enabled, macOS will prompt you to allow
          Mouseless background access and Accessibility permissions.
        '';
      };

      keepAlive = mkOption {
        type = types.bool;
        default = true;
        description = "Whether the launchd service should be kept alive.";
      };
    };

    systemd = {
      enable = mkEnableOption "the systemd user service for Mouseless" // {
        default = true;
      };

      target = mkOption {
        type = types.str;
        default = "graphical-session.target";
        description = "The systemd target for the Mouseless service.";
      };

      wantedBy = mkOption {
        type = types.listOf types.str;
        default = [ "default.target" ];
        description = "The systemd units that want the Mouseless service.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    launchd.agents.mouseless = lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin && cfg.launchd.enable) {
      enable = true;
      config = {
        Program = "${cfg.package}/Applications/Mouseless.app/Contents/MacOS/mouseless";
        KeepAlive = cfg.launchd.keepAlive;
        RunAtLoad = true;
        StandardOutPath = "/tmp/mouseless.log";
        StandardErrorPath = "/tmp/mouseless.err.log";
      };
    };

    systemd.user.services.mouseless =
      lib.mkIf (cfg.systemd.enable && config ? systemd.user)
        {
          Unit = {
            Description = "Mouseless - keyboard-driven mouse control";
            PartOf = [ cfg.systemd.target ];
            After = [ cfg.systemd.target ];

          };

          Service = {
            ExecStart = lib.getExe cfg.package;
            Restart = "on-failure";
            RestartSec = 5;
            Slice = "app.slice";
          };

          Install = {
            WantedBy = cfg.systemd.wantedBy;
          };
        };
  };
}
