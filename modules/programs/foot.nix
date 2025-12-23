{ config, lib, pkgs, ... }:

let
  cfg = config.programs.foot;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  footLauncher = config.lib.machine.wrapLauncher cfg.package;
in
{
  programs.foot = {
    enable = true;

    settings = {
      main = {
        shell = config.machine.interactiveShell;
        term = "xterm-256color";
        font = "PragmataPro Mono Liga:size=12";
        bold-text-in-bright = "yes";
        dpi-aware = "no";
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      terminal = footLauncher;
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${footLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${footLauncher}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf (cfg.enable && fuzzelcfg.enable) {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
