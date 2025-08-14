{ config, lib, pkgs, ... }:

let
  cfg = config.programs.wezterm;
in
{
  programs.wezterm = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.unstable.wezterm;

    extraConfig = ''
      local config = wezterm.config_builder()
      local shell = "${lib.getExe config.programs.fish.package}"
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

      config.window_padding = {
        left = '0.2cell',
        right = '1cell',
        top = '0',
        bottom = '0',
      }

      config.initial_cols = 120
      config.initial_rows = 36

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

      return config
    '';
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

  programs.niri =
    {
      settings = {
        binds = {
          "Mod+T".action.spawn = [
            "${lib.getExe cfg.package}"
          ];
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
