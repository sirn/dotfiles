{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf replaceStrings concatStringsSep;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs) formats;

  cfg = config.programs.alacritty;

  tomlFormat = formats.toml { };

  alacrittyBin =
    if config.machine.isNixOS || isDarwin
    then "${cfg.package}/bin/alacritty"
    else "alacritty";
in
{
  programs.alacritty = {
    # Alacritty depends on GL stuff, which means we can't use Nix packages
    # on a non-NixOS or non-Darwin due to a library mismatch.
    enable = config.machine.isNixOS || isDarwin;

    settings = {
      window =
        if isDarwin
        then { option_as_alt = "Both"; }
        else { };

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
          background = "#242424";
          foreground = "#ffffff";
        };

        normal = {
          black = "#242424";
          red = "#f62b5a";
          green = "#47b413";
          yellow = "#e3c401";
          blue = "#24acd4";
          magenta = "#f2affd";
          cyan = "#13c299";
          white = "#e6e6e6";
        };

        bright = {
          black = "#616161";
          red = "#ff4d51";
          green = "#35d450";
          yellow = "#e9e836";
          blue = "#5dc5f8";
          magenta = "#feabf2";
          cyan = "#24dfc4";
          white = "#ffffff";
        };
      };

      hints = {
        enabled = [
          {
            command =
              if isDarwin
              then "open"
              else "${pkgs.xdg-utils}/bin/xdg-open";

            hyperlinks = true;
            post_processing = true;
            persist = false;

            mouse = {
              enabled = true;
              mods = "Control";
            };

            binding = {
              key = "J"; # Ctrl+Shift+U is being used to enter Unicode literal with Fcitx
              mods = "Control|Shift";
            };

            regex = "(https://|http://)[^\\u0000-\\u001F\\u007F-\\u009F<>\"\\\\s{-}\\\\^⟨⟩`]+";
          }
        ];
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
  xdg.configFile = mkIf
    (!cfg.enable)
    {
      "alacritty/alacritty.toml" = mkIf (cfg.settings != { }) {
        source =
          (tomlFormat.generate "alacritty.toml" cfg.settings).overrideAttrs
            (finalAttrs: prevAttrs: {
              buildCommand = concatStringsSep "\n" [
                prevAttrs.buildCommand
                "substituteInPlace $out --replace '\\\\' '\\'"
              ];
            });
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
