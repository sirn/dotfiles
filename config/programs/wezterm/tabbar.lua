local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.use_fancy_tab_bar = false
config.tab_max_width = 32
config.show_new_tab_button_in_tab_bar = false
config.colors = {
  tab_bar = {
    background = 'transparent',
  },
}

local function get_theme_colors_from_config(cfg)
  local scheme_name = cfg.color_scheme

  if cfg.color_schemes and cfg.color_schemes[scheme_name] then
    return cfg.color_schemes[scheme_name]
  end

  -- Fallback built-in schemes
  local builtins = wezterm.color.get_builtin_schemes()
  if builtins[scheme_name] then
    return builtins[scheme_name]
  end

  return cfg.colors
end

local function tab_title(tab_info)
  local title = tab_info.tab_title
  if title and #title > 0 then
    return title
  end
  return tab_info.active_pane.title
end

wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    local index = tab.tab_index + 1
    local title = tab_title(tab)
    title = wezterm.truncate_right(title, max_width - 6)

    local colors = get_theme_colors_from_config(config)
    if not colors or not colors.ansi then
      return { Text = ' ' .. index .. ':' .. title .. ' ' }
    end

    local background = colors.background or '#ffffff'
    local foreground = colors.foreground or '#000000'
    local blue = colors.ansi[5] or '#0000ff'
    local black = colors.ansi[1] or '#000000'
    local gray = colors.brights[1] or '#383838'
    local white = colors.brights[8] or '#ffffff'

    local parts = {}

    if tab.is_active then
      table.insert(parts, { Foreground = { Color = blue } })
      table.insert(parts, { Background = { Color = background } })
      table.insert(parts, { Text = "┃" })

      table.insert(parts, { Background = { Color = blue } })
      table.insert(parts, { Foreground = { Color = white } })
      table.insert(parts, { Text = " " .. index .. " " })

      table.insert(parts, { Background = { Color = gray } })
      table.insert(parts, { Foreground = { Color = white } })
      table.insert(parts, { Text = " " .. title .. " " })
    else
      table.insert(parts, { Background = { Color = background } })
      table.insert(parts, { Text = " " })

      table.insert(parts, { Background = { Color = gray } })
      table.insert(parts, { Foreground = { Color = white } })
      table.insert(parts, { Text = " " .. index .. " " })

      table.insert(parts, { Background = { Color = background } })
      table.insert(parts, { Foreground = { Color = foreground } })
      table.insert(parts, { Text = " " .. title .. " " })
    end

    return parts
  end
)

wezterm.on(
  'update-status',
  function(window, pane)
    local domain = pane:get_domain_name()
    local hostname = wezterm.hostname()
    -- Strip domain from hostname for cleaner look
    hostname = hostname:match("^([^%.]+)") or hostname

    local text = ""
    if domain == 'local' or domain == 'default' then
       -- Try to get workspace, fallback to domain
       local workspace = window:active_workspace()
       text = workspace .. "@" .. hostname
    else
       text = domain .. "@" .. hostname
    end

    local time = wezterm.strftime("%H:%M")
    local colors = get_theme_colors_from_config(window:effective_config())

    if not colors or not colors.ansi then
      window:set_right_status(wezterm.format({ { Text = " " .. text .. " " .. time .. " " } }))
      return
    end

    local background = colors.background or '#ffffff'
    local foreground = colors.foreground or '#000000'
    local blue = colors.ansi[5] or '#0000ff'
    local black = colors.ansi[1] or '#000000'
    local gray = colors.brights[1] or '#383838'
    local white = colors.brights[8] or '#ffffff'

    local parts = {}

    -- Divider
    table.insert(parts, { Foreground = { Color = blue } })
    table.insert(parts, { Background = { Color = background } })
    table.insert(parts, { Text = "┃" })

    -- Session@Host
    table.insert(parts, { Foreground = { Color = blue } })
    table.insert(parts, { Background = { Color = background } })
    table.insert(parts, { Text = " " .. text .. " " })

    -- Time
    table.insert(parts, { Foreground = { Color = white } })
    table.insert(parts, { Background = { Color = gray } })
    table.insert(parts, { Text = " " .. time .. " " })

    window:set_right_status(wezterm.format(parts))
  end
)

return config
