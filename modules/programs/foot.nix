{ config, lib, pkgs, ... }:

let
  cfg = config.programs.foot;

  swaycfg = config.wayland.windowManager.sway.config;

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

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = footLauncher;
      keybindings = {
        "${swaycfg.modifier}+Return" = "exec ${footLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${footLauncher}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf fuzzelcfg.enable {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
