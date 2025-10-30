{ config, lib, pkgs, ... }:

let
  cfg = config.programs.wezterm;

  fishcfg = config.programs.fish;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;
in
{
  programs.wezterm = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.unstable.wezterm;

    extraConfig = ''
      local act = wezterm.action
      local config = wezterm.config_builder()
      local shell = "${config.machine.interactiveShell}"
      local font = wezterm.font_with_fallback({
        'PragmataPro Mono Liga',
        'Source Han Code JP',
      })

      config.color_scheme = 'default'
      config.enable_scroll_bar = true
      config.font = font

      local version_prefix = string.sub(wezterm.version, 1, 6)
      local version_num = tonumber(version_prefix) or 0
      if version_num >= 202505 then
        config.command_palette_font = font
        config.pane_select_font = font
        config.char_select_font = font
      end

      config.freetype_load_target = 'Light'
      config.freetype_load_flags = 'NO_HINTING'
      config.hide_tab_bar_if_only_one_tab = true
      config.use_ime = true
      config.mux_enable_ssh_agent = false
      config.warn_about_missing_glyphs = false

      config.window_decorations = "RESIZE"
      config.window_padding = {
        left = '1cell',
        right = '0.5cell',
        top = '0.5cell',
        bottom = '0',
      }

      config.keys = {
        { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
      }

      ${lib.optionalString pkgs.stdenv.isLinux ''
        config.default_prog = { shell };
        config.font_size = 12.0
        config.command_palette_font_size = 12.0
        config.pane_select_font_size = 12.0
        config.char_select_font_size = 12.0
      ''}

      ${lib.optionalString pkgs.stdenv.isDarwin ''
        config.default_prog = { shell, "--login" }
        config.font_size = 14.0;
        config.command_palette_font_size = 14.0
        config.pane_select_font_size = 14.0
        config.char_select_font_size = 14.0
      ''}

      config.use_fancy_tab_bar = false

      return config
    '';
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = lib.getExe cfg.package;
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${lib.getExe cfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${lib.getExe cfg.package}"
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
