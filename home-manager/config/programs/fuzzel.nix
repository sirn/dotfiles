{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;
in
{
  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        font = "monospace:size=12";
        dpi-aware = "no";
        width = "40";
        line-height = "18";
        horizontal-pad = "8";
        vertical-pad = "4";
        layer = "overlay";
        render-workers = 4;
        match-workers = 4;
      };

      border = {
        radius = "4";
        width = "4";
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      keybindings = {
        "${swaycfg.config.modifier}+d" = "exec ${cfg.package}/bin/fuzzel";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+d".action.spawn = [ "${lib.getExe cfg.package}" ];
      };
    };
  };
}
