{ config, lib, pkgs, ... }:

let
  weztermBin =
    if config.machine.isNixOS
    then "${config.programs.wezterm.package}/bin/wezterm"
    else "wezterm";
in
{
  programs.wezterm = {
    enable = config.machine.isNixOS;

    # On non-NixOS, this should be installed using OS package manager.
    package = pkgs.unstable.wezterm;

    colorSchemes = {
      foot = {
        ansi = [
          "#242424"
          "#f62b5a"
          "#47b413"
          "#e3c401"
          "#24acd4"
          "#f2affd"
          "#13c299"
          "#e6e6e6"
        ];
        brights = [
          "#616161"
          "#ff4d51"
          "#35d450"
          "#e9e836"
          "#5dc5f8"
          "#feabf2"
          "#24dfc4"
          "#ffffff"
        ];

        background = "#242424";
        cursor_bg = "#ffffff";
        cursor_border = "#ffffff";
        cursor_fg = "#242424";
        foreground = "#ffffff";
        scrollbar_thumb = "#d1d1d1";
        selection_bg = "#d1d1d1";
        selection_fg = "#242424";
      };
    };

    extraConfig = ''
      local config = wezterm.config_builder()
      local shell = "${config.programs.fish.package}/bin/fish"
      local font = wezterm.font_with_fallback({
        'PragmataPro Mono Liga',
        'Source Han Code JP',
      })

      config.color_scheme = 'foot'
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

  # Hack until we can use null package in Home Manager 25.04
  xdg.configFile =
    let
      cfg = config.programs.wezterm;

      tomlFormat = pkgs.formats.toml { };

      shellIntegrationStr = ''
        source "/etc/profile.d/wezterm.sh"
      '';
    in
    (lib.mkIf (!config.machine.isNixOS) (lib.mkMerge [
      {
        "wezterm/wezterm.lua".text = ''
          -- Generated by Home Manager.
          -- See https://wezfurlong.org/wezterm/

          local wezterm = require 'wezterm'

          ${cfg.extraConfig}
        '';
      }
      (lib.mapAttrs'
        (name: value:
          lib.nameValuePair "wezterm/colors/${name}.toml" {
            source = tomlFormat.generate "${name}.toml" { colors = value; };
          })
        cfg.colorSchemes)
    ]));

  wayland.windowManager.sway =
    let
      swaycfg = config.wayland.windowManager.sway.config;
    in
    {
      config = {
        terminal = weztermBin;
        keybindings = {
          "${swaycfg.modifier}+Return" = "exec ${weztermBin}";
        };
      };
    };

  programs.fuzzel = {
    settings = {
      main = {
        terminal = weztermBin;
      };
    };
  };
}
