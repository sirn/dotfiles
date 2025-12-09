{ lib, config, pkgs, ... }:

let
  cfg = config.programs.ghostty;

  swaycfg = config.wayland.windowManager.sway.config;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  ghosttyLauncher = config.lib.machine.wrapLauncher cfg.package;
in
{
  programs.ghostty = {
    enable = true;

    package = config.lib.nixGL.wrap pkgs.unstable.ghostty;

    settings = {
      font-family = "PragmataPro Mono Liga";

      command = builtins.concatStringsSep " " (
        [ config.machine.interactiveShell ]
        ++ lib.optional pkgs.stdenv.isDarwin "--login"
      );

      font-size =
        if pkgs.stdenv.isDarwin
        then 14
        else 12;
    };
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = ghosttyLauncher;
      keybindings = {
        "${swaycfg.modifier}+Return" = "exec ${ghosttyLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${ghosttyLauncher}"
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
