{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.tiler;

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
  };

  config = lib.mkIf cfg.enable {
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
