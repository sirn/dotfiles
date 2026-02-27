local wezterm = require 'wezterm'
local config = wezterm.config_builder()

local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil

local font = wezterm.font_with_fallback({
  'PragmataPro Mono Liga',
  'Source Han Code JP',
})

config.font = font
config.command_palette_font = font
config.pane_select_font = font
config.char_select_font = font
config.warn_about_missing_glyphs = false

if is_darwin then
  config.font_size = 14.0
  config.command_palette_font_size = 14.0
  config.pane_select_font_size = 14.0
  config.char_select_font_size = 14.0
else
  config.font_size = 12.0
  config.command_palette_font_size = 12.0
  config.pane_select_font_size = 12.0
  config.char_select_font_size = 12.0
end

config.use_ime = true
config.freetype_load_target = 'Light'
config.freetype_load_flags = 'NO_HINTING'

return config
