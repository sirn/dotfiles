{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.programs.mouseless;
in {
  options.programs.mouseless = {
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
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    launchd.agents.mouseless = lib.mkIf cfg.launchd.enable {
      enable = true;
      config = {
        Program = "${cfg.package}/Applications/Mouseless.app/Contents/MacOS/mouseless";
        KeepAlive = cfg.launchd.keepAlive;
        RunAtLoad = true;
        StandardOutPath = "/tmp/mouseless.log";
        StandardErrorPath = "/tmp/mouseless.err.log";
      };
    };
  };
}
