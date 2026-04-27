{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.foot;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  fontcfg = config.home.fonts;

  noctaliaShellCfg = config.programs.noctalia-shell;

  footLauncher = config.lib.home.wrapLauncher cfg.package;
in
{
  programs.foot = {
    enable = true;

    settings = {
      main = {
        shell = config.home.shell.interactiveShell;
        term = "xterm-256color";
        font = "${fontcfg.terminal.monospace}:size=${toString fontcfg.terminal.size}";
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
        "Mod+T".action.spawn = [ "${footLauncher}" ];
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

  programs.noctalia-shell = lib.mkIf noctaliaShellCfg.enable {
    settings = {
      appLauncher = {
        terminalCommand = lib.getExe cfg.package;
      };
    };
  };
}
