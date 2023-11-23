{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.desktop.enable {
  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        font = "monospace:size=12";
        dpi-aware = "no";
        terminal = "foot -e";
        width = "40";
        line-height = "18";
        horizontal-pad = "8";
        vertical-pad = "4";
        layer = "overlay";
      };

      colors = {
        background = "1e2225fa";
        selection = "285577ff";
        border = "494e52ff";
        text = "999999ff";
        match = "ffffffff";
        selection-text = "ddddddff";
        selection-match = "ffffffff";
      };

      border = {
        radius = "0";
      };
    };
  };
}
