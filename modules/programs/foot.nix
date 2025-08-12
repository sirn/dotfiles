{ config, lib, pkgs, ... }:

let
  cfg = config.programs.foot;
in
{
  programs.foot = {
    enable = true;

    settings = {
      main = {
        shell = "${config.programs.fish.package}/bin/fish";
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
    {
      config = {
        terminal = footBin;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${footBin}";
        };
      };
    };

  programs.fuzzel = {
    settings = {
      main = {
        terminal = "${cfg.package}/bin/foot";
      };
    };
  };
}
