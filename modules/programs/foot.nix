{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.desktop.enable {
  programs.foot = {
    enable = true;

    settings = {
      main = {
        term = "xterm-256color";
        font = "PragmataPro Mono:size=12";
        bold-text-in-bright = "yes";
        dpi-aware = "no";
      };

      colors = {
        background = "232627";
        foreground = "eff0f1";
        regular0 = "232627";
        regular1 = "ed1515";
        regular2 = "11d116";
        regular3 = "f67400";
        regular4 = "1d99f3";
        regular5 = "9b59b6";
        regular6 = "1abc9c";
        regular7 = "c5c8c6";
        bright0 = "7f8c8d";
        bright1 = "c0392b";
        bright2 = "1cdc9a";
        bright3 = "fdbc4b";
        bright4 = "3daee9";
        bright5 = "8e44ad";
        bright6 = "16a085";
        bright7 = "fcfcfc";
      };
    };
  };

  wayland.windowManager.sway = mkIf config.programs.foot.enable {
    config = {
      terminal = "${config.programs.foot.package}/bin/foot";
    };
  };

  programs.fuzzel = mkIf config.programs.foot.enable {
    terminal = "${config.programs.foot.package}/bin/foot";
  };
}
