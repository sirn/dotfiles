{ config, lib, pkgs, ... }:

let
  cfg = config.programs.alacritty;
in
{
  programs.alacritty = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.alacritty;

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

  wayland.windowManager.sway =
    let
      swaycfg = config.wayland.windowManager.sway.config;
    in
    {
      config = {
        terminal = lib.getExe cfg.package;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${lib.getExe cfg.package}";
        };
      };
    };

  programs.fuzzel = {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
