{ lib, config, pkgs, ... }:

let
  cfg = config.programs.ghostty;

  swaycfg = config.wayland.windowManager.sway;

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

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      terminal = ghosttyLauncher;
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${ghosttyLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${ghosttyLauncher}"
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
