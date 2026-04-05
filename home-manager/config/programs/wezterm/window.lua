local wezterm = require 'wezterm'

local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil

local config = {
  enable_scroll_bar = true,
}

if is_darwin then
  config.window_decorations = "RESIZE"
  config.window_padding = {
    left = '1cell',
    right = '0.5cell',
    top = '0.5cell',
    bottom = '0',
  }
else
  config.window_padding = {
    left = '0.5cell',
    right = '1cell',
    top = '0',
    bottom = '0',
  }
end

return config
