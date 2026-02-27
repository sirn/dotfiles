local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.use_fancy_tab_bar = false
config.tab_max_width = 32
config.show_new_tab_button_in_tab_bar = false

local colors = {
  active_index_bg = 'blue',
  active_index_fg = 'black',
  active_title_bg = 'grey',
  active_title_fg = 'white',
  inactive_index_bg = 'grey',
  inactive_index_fg = 'white',
  inactive_title_bg = 'black',
  inactive_title_fg = 'white',
  status_bg = 'grey',
  status_fg = 'white',
  status_icon_bg = 'black',
  status_icon_fg = 'blue',
}

local hm_colors_ok, hm_colors = pcall(require, '../hm_colors')
if hm_colors_ok and hm_colors.tab_colors then
  for k, v in pairs(hm_colors.tab_colors) do
    colors[k] = v
  end
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

    local parts = {}

    if tab.is_active then
      table.insert(parts, { Foreground = { Color = colors.active_index_bg } })
      table.insert(parts, { Background = { Color = colors.inactive_title_bg } })
      table.insert(parts, { Text = "┃" })

      table.insert(parts, { Background = { Color = colors.active_index_bg } })
      table.insert(parts, { Foreground = { Color = colors.active_index_fg } })
      table.insert(parts, { Text = " " .. index .. " " })

      table.insert(parts, { Background = { Color = colors.active_title_bg } })
      table.insert(parts, { Foreground = { Color = colors.active_title_fg } })
      table.insert(parts, { Text = " " .. title .. " " })
    else
      table.insert(parts, { Background = { Color = colors.inactive_title_bg } })
      table.insert(parts, { Text = " " })

      table.insert(parts, { Background = { Color = colors.inactive_index_bg } })
      table.insert(parts, { Foreground = { Color = colors.inactive_index_fg } })
      table.insert(parts, { Text = " " .. index .. " " })

      table.insert(parts, { Background = { Color = colors.inactive_title_bg } })
      table.insert(parts, { Foreground = { Color = colors.inactive_title_fg } })
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

    local parts = {}

    -- Divider
    table.insert(parts, { Foreground = { Color = colors.status_icon_fg } })
    table.insert(parts, { Background = { Color = colors.status_icon_bg } })
    table.insert(parts, { Text = "┃" })

    -- Session@Host
    table.insert(parts, { Foreground = { Color = colors.status_icon_fg } })
    table.insert(parts, { Background = { Color = colors.status_icon_bg } })
    table.insert(parts, { Text = " " .. text .. " " })

    -- Time
    table.insert(parts, { Foreground = { Color = colors.status_fg } })
    table.insert(parts, { Background = { Color = colors.status_bg } })
    table.insert(parts, { Text = " " .. time .. " " })

    window:set_right_status(wezterm.format(parts))
  end
)

return config
