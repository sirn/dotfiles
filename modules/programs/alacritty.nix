{ config, lib, pkgs, ... }:

let
  cfg = config.programs.alacritty;

  swaycfg = config.wayland.windowManager.sway.config;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  alacrittyMaybeUwsm = pkgs.writeShellScript "alacritty" ''
    if command -v uwsm >/dev/null; then
      exec uwsm app -- ${lib.getExe cfg.package}
    else
      exec ${lib.getExe cfg.package}
    fi
  '';
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

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = alacrittyMaybeUwsm;
      keybindings = {
        "${swaycfg.modifier}+Return" = "exec ${alacrittyMaybeUwsm}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${alacrittyMaybeUwsm}"
        ];
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
