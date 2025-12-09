{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  uwsmFinalize = pkgs.writeShellScript "uwsm-finalize" ''
    if command -v uwsm >/dev/null; then
      exec uwsm finalize
    fi
  '';
in
{
  wayland.windowManager.sway.config = lib.mkIf swaycfg.enable {
    startup = [
      { command = "${uwsmFinalize}"; }
    ];
  };

  programs.fuzzel.settings = lib.mkIf fuzzelcfg.enable {
    main = {
      launch-prefix = "uwsm app --";
    };
  };
}
