{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.wezterm;

  fishcfg = config.programs.fish;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;

  fontcfg = config.home.fonts;

  weztermLauncher = config.lib.home.wrapLauncher cfg.package;
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
    "wezterm/modules/fonts.lua".text = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()

      local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil

      local font = wezterm.font_with_fallback({
        '${fontcfg.terminal.monospace}',
        'Source Han Code JP',
        'Symbols Nerd Font',
      })

      config.font = font
      config.command_palette_font = font
      config.pane_select_font = font
      config.char_select_font = font
      config.warn_about_missing_glyphs = false

      local font_size = ${toString fontcfg.terminal.size}

      if is_darwin then
        config.font_size = font_size
        config.command_palette_font_size = font_size
        config.pane_select_font_size = font_size
        config.char_select_font_size = font_size
      else
        config.font_size = font_size
        config.command_palette_font_size = font_size
        config.pane_select_font_size = font_size
        config.char_select_font_size = font_size
      end

      config.use_ime = true
      config.freetype_load_target = 'Light'
      config.freetype_load_flags = 'NO_HINTING'

      return config
    '';

    "wezterm/modules/term.lua" = {
      text = ''
        return {
          term = "xterm-256color",
          enable_kitty_keyboard = true,
        }
      '';
    };

    "wezterm/modules/shell.lua" = {
      text = ''
        return {
          default_prog = {
            "${config.home.shell.interactiveShell}",
            ${lib.optionalString pkgs.stdenv.isDarwin ''
              "--login",
            ''}
          }
        }
      '';
    };

    "wezterm/modules/colors.lua".text = lib.mkIf config.programs.wezterm.enable ''
      return {
        color_scheme = '${config.home.colors.themeName}',
      }
    '';

    "wezterm/modules/tabbar.lua".text = builtins.readFile ./tabbar.lua;

    "wezterm/modules/mux.lua".text = builtins.readFile ./mux.lua;

    "wezterm/modules/window.lua".text = builtins.readFile ./window.lua;

    "wezterm/modules/keybindings.lua".text = builtins.readFile ./keybindings.lua;

    "wezterm/modules/theme-switcher.lua".text = builtins.readFile ./theme-switcher.lua;

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
        "Mod+T".action.spawn = [ "${weztermLauncher}" ];
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
