{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (lib) optionalString;
in
{
  programs.wezterm = {
    enable = true;
    package = pkgs.nightlies.wezterm;

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

      config.color_scheme = 'foot'
      config.enable_scroll_bar = true
      config.font = wezterm.font 'PragmataPro Mono'
      config.freetype_load_target = 'Light'
      config.freetype_load_flags = 'NO_HINTING'
      config.hide_tab_bar_if_only_one_tab = true
      config.prefer_egl = true
      config.use_ime = true

      config.window_padding = {
        left = '0.2cell',
        right = '1cell',
        top = '0',
        bottom = '0',
      }

      ${optionalString isLinux ''
        config.default_prog = { shell };
        config.font_size = 12.0
        config.front_end = "WebGpu"
        config.enable_wayland = true
      ''}

      ${optionalString isDarwin ''
        config.default_prog = { shell, "--login" }
        config.font_size = 14.0;
      ''}

      return config
    '';
  };
}
