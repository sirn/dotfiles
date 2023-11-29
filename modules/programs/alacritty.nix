{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isDarwin;
in
mkIf config.desktop.enable {
  programs.alacritty = {
    enable = true;

    settings = {
      font = {
        size = 14.0;
        normal = {
          family = "PragmataPro Mono";
        };
      };

      colors = {
        primary = {
          background = "#232627";
          foreground = "#eff0f1";
        };

        normal = {
          black = "#232627";
          red = "#ed1515";
          green = "#11d116";
          yellow = "#f67400";
          blue = "#1d99f3";
          magenta = "#9b59b6";
          cyan = "#1abc9c";
          white = "#c5c8c6";
        };

        bright = {
          black = "#7f8c8d";
          red = "#c0392b";
          green = "#1cdc9a";
          yellow = "#fdbc4b";
          blue = "#3daee9";
          magenta = "#8e44ad";
          cyan = "#16a085";
          white = "#fcfcfc";
        };
      };

      shell = {
        program = "${config.programs.zsh.package}/bin/zsh";
        args =
          if isDarwin
          then [ "--login" ]
          else [ ];
      };
    };
  };

  wayland.windowManager.sway = mkIf config.programs.alacritty.enable {
    config = {
      terminal = "${config.programs.alacritty.package}/bin/alacritty";
    };
  };

  programs.fuzzel = mkIf config.programs.alacritty.enable {
    terminal = "${config.programs.alacritty.package}/bin/alacritty";
  };
}
