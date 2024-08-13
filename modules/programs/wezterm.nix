{ config, ... }:

{
  programs.wezterm = {
    enable = true;

    colorSchemes = {
      foot = {
        ansi = [ "#242424" "#f62b5a" "#47b413" "#e3c401" "#24acd4" "#f2affd" "#13c299" "#e6e6e6" ];
        brights = [ "#616161" "#ff4d51" "#35d450" "#e9e836" "#5dc5f8" "#feabf2" "#24dfc4" "#ffffff" ];
        background = "#242424";
        cursor_bg = "#ffffff";
        cursor_border = "#ffffff";
        cursor_fg = "#242424";
        foreground = "#ffffff";
        selection_bg = "#00ced1";
        selection_fg = "#000000";
      };
    };

    extraConfig = ''
      local config = wezterm.config_builder()
      local shell = "${config.programs.zsh.package}/bin/zsh"
      local is_linux <const> = wezterm.target_triple:find("linux") ~= nil
      local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil

      config.color_scheme = 'foot'
      config.default_prog = is_darwin and { shell, "--login" } or { shell };
      config.font = wezterm.font 'PragmataPro Mono'
      config.freetype_load_target = "Light"
      config.font_size = is_darwin and 14.0 or 12.0
      config.hide_tab_bar_if_only_one_tab = true
      config.prefer_egl = true;
      config.use_ime = true;

      return config
    '';
  };
}
