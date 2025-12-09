{ lib, config, pkgs, ... }:

let
  cfg = config.programs.ghostty;

  swaycfg = config.wayland.windowManager.sway.config;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  ghosttyMaybeUwsm = pkgs.writeShellScript "ghostty" ''
    if command -v uwsm >/dev/null; then
      exec uwsm app -- ${lib.getExe cfg.package}
    else
      exec ${lib.getExe cfg.package}
    fi
  '';
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
      terminal = ghosttyMaybeUwsm;
      keybindings = {
        "${swaycfg.modifier}+Return" = "exec ${ghosttyMaybeUwsm}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${ghosttyMaybeUwsm}"
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
