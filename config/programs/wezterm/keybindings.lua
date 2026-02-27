local wezterm = require 'wezterm'
return {
  keys = {
    {
      key = 'Enter',
      mods = 'SHIFT',
      action = wezterm.action({SendString="\x1b\r"})
    }
  }
}
