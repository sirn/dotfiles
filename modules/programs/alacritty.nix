{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf replaceStrings;
  inherit (pkgs.stdenv) isDarwin;

  cfg = config.programs.alacritty;

  alacrittyBin =
    if config.machine.isNixOS || isDarwin
    then "${cfg.package}/bin/alacritty"
    else "alacritty";
in
mkIf config.desktop.enable {
  programs.alacritty = {
    # Alacritty depends on GL stuff, which means we can't use Nix packages
    # on a non-NixOS due to library mismatch.
    enable = config.machine.isNixOS || isDarwin;

    settings = {
      window = {
        option_as_alt = "Both";
      };

      font = {
        size =
          if isDarwin
          then 14.0
          else 12.0;

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

  # For non-NixOS and non-Darwin, only configure.
  xdg.configFile = mkIf (!cfg.enable) {
    "alacritty/alacritty.yml" = mkIf (cfg.settings != { }) {
      text = replaceStrings [ "\\\\" ] [ "\\" ] (builtins.toJSON cfg.settings);
    };
  };

  wayland.windowManager.sway =
    let
      swaycfg = config.wayland.windowManager.sway.config;
    in
    {
      config = {
        terminal = alacrittyBin;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${alacrittyBin}";
        };
      };
    };

  programs.fuzzel = {
    settings = {
      main = {
        terminal = alacrittyBin;
      };
    };
  };
}
