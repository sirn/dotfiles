{ config, lib, pkgs, ... }:

let
  cfg = config.programs.alacritty;

  fuzzelcfg = config.programs.fuzzel;
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
          family = "PragmataPro Mono Liga";
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
        program = config.machine.interactiveShell;
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
    lib.mkIf swaycfg.enable {
      config = {
        terminal = lib.getExe cfg.package;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${lib.getExe cfg.package}";
        };
      };
    };

  programs.fuzzel = lib.mkIf fuzzelcfg.enable {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
