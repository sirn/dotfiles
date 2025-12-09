{ config, lib, pkgs, ... }:

let
  cfg = config.programs.foot;

  swaycfg = config.wayland.windowManager.sway.config;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  footMaybeUwsm = pkgs.writeShellScript "foot" ''
    if command -v uwsm >/dev/null; then
      exec uwsm app -- ${lib.getExe cfg.package}
    else
      exec ${lib.getExe cfg.package}
    fi
  '';
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
      terminal = footMaybeUwsm;
      keybindings = {
        "${swaycfg.modifier}+Return" = "exec ${footMaybeUwsm}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${footMaybeUwsm}"
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
