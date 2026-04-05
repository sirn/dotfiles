local wezterm = require 'wezterm'
local act = wezterm.action

local function get_display_name(filename)
  return filename:gsub("%.toml$", ""):gsub("-", " ")
end

local function get_scheme_name(filename)
  return filename:gsub("%.toml$", "")
end

local function get_themes()
  local colors_dir = wezterm.config_dir .. '/colors'
  local entries = {}

  local p = io.popen('ls -1 "' .. colors_dir .. '" 2>/dev/null')
  if not p then
    return entries
  end

  for file in p:lines() do
    if file:match('%.toml$') then
      table.insert(entries, {
        id = get_scheme_name(file),
        label = get_display_name(file),
      })
    end
  end
  p:close()

  table.sort(entries, function(a, b)
    return a.label < b.label
  end)

  return entries
end

local themes = get_themes()

wezterm.on('augment-command-palette', function(window, pane)
  return {
    {
      brief = 'Switch Theme',
      icon = 'md_palette',
      action = act.InputSelector {
        title = 'Select Color Theme',
        choices = themes,
        action = wezterm.action_callback(function(inner_window, inner_pane, id, label)
          if not id then
            return
          end

          local overrides = inner_window:get_config_overrides() or {}
          overrides.color_scheme = id

          inner_window:set_config_overrides(overrides)
        end),
      },
    },
  }
end)

return {}
