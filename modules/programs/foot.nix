{ config, lib, pkgs, ... }:

let
  cfg = config.programs.foot;

  fishcfg = config.programs.fish;

  fuzzelcfg = config.programs.fuzzel;
in
{
  programs.foot = {
    enable = true;

    settings = {
      main = {
        shell = "${fishcfg.package}/bin/fish";
        term = "xterm-256color";
        font = "PragmataPro Mono:size=12";
        bold-text-in-bright = "yes";
        dpi-aware = "no";
      };
    };
  };

  wayland.windowManager.sway =
    let
      swaycfg = config.wayland.windowManager.sway.config;
      footBin = "${cfg.package}/bin/foot";
    in
    lib.mkIf swaycfg.enable {
      config = {
        terminal = footBin;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${footBin}";
        };
      };
    };

  programs.fuzzel = lib.mkIf fuzzelcfg.enable {
    settings = {
      main = {
        terminal = "${cfg.package}/bin/foot";
      };
    };
  };
}
