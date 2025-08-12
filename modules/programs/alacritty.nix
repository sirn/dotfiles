{ config, lib, pkgs, ... }:

let
  cfg = config.programs.alacritty;

  tomlFormat = pkgs.formats.toml { };

  alacrittyBin =
    if config.machine.isNixOS || pkgs.stdenv.isDarwin
    then "${cfg.package}/bin/alacritty"
    else "alacritty";
in
{
  programs.alacritty = {
    # Alacritty depends on GL stuff, which means we can't use Nix packages
    # on a non-NixOS or non-Darwin due to a library mismatch.
    enable = config.machine.isNixOS || pkgs.stdenv.isDarwin;

    settings = {
      window =
        if pkgs.stdenv.isDarwin
        then { option_as_alt = "Both"; }
        else { };

      font = {
        size =
          if pkgs.stdenv.isDarwin
          then 14.0
          else 12.0;

        normal = {
          family = "PragmataPro Mono";
        };
      };

      hints = {
        enabled = [
          {
            command =
              if pkgs.stdenv.isDarwin
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
        program = "${config.programs.fish.package}/bin/fish";
        args =
          if pkgs.stdenv.isDarwin
          then [ "--login" ]
          else [ ];
      };
    };
  };

  # For non-NixOS and non-Darwin, only configure.
  xdg.configFile = lib.mkIf
    (!cfg.enable)
    {
      "alacritty/alacritty.toml" = lib.mkIf (cfg.settings != { }) {
        source =
          (tomlFormat.generate "alacritty.toml" cfg.settings).overrideAttrs
            (finalAttrs: prevAttrs: {
              buildCommand = lib.concatStringsSep "\n" [
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
