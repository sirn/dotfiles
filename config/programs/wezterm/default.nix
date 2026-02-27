{ config, lib, pkgs, ... }:

let
  cfg = config.programs.wezterm;

  fishcfg = config.programs.fish;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;

  weztermLauncher = config.lib.machine.wrapLauncher cfg.package;
in
{
  programs.wezterm = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.wezterm;

    extraConfig = builtins.readFile ./wezterm.lua;
  };

  xdg.configFile = {
    "wezterm/modules/fonts.lua".text = builtins.readFile ./fonts.lua;

    "wezterm/modules/term.lua" = {
      text = ''
        return {
          term = "wezterm",
        }
      '';
    };

    "wezterm/modules/shell.lua" = {
      text = ''
        return {
          default_prog = {
            "${config.machine.interactiveShell}",
            ${lib.optionalString pkgs.stdenv.isDarwin ''
              "--login",
            ''}
          }
        }
      '';
    };

    "wezterm/modules/tabbar.lua".text = builtins.readFile ./tabbar.lua;

    "wezterm/modules/mux.lua".text = builtins.readFile ./mux.lua;

    "wezterm/modules/window.lua".text = builtins.readFile ./window.lua;

    "wezterm/modules/keybindings.lua".text = builtins.readFile ./keybindings.lua;

    # Workaround for https://github.com/wezterm/wezterm/issues/6685
    # Clipboard not working between terminals on Wayland
    "wezterm/modules/workaround-6685.lua" = lib.mkIf (swaycfg.enable || niricfg.enable) {
      text = ''
        local wezterm = require 'wezterm'

        wezterm.on(
          'window-focus-changed',
          function(window, pane)
            wezterm.run_child_process {
              'sh', '-c',
              '${pkgs.wl-clipboard}/bin/wl-paste -n | ${pkgs.wl-clipboard}/bin/wl-copy'
            }
          end
        )

        return {}
      '';
    };
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      terminal = "${weztermLauncher}";
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${weztermLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${weztermLauncher}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf (cfg.enable && fuzzelcfg.enable) {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
