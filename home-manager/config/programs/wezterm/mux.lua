local wezterm = require 'wezterm'

wezterm.on(
  'update-status',
  function(window, pane)
    local domain = pane:get_domain_name()
    local overrides = window:get_config_overrides() or {}

    if domain == 'local' or domain == 'default' then
      overrides.hide_tab_bar_if_only_one_tab = true
    else
      overrides.hide_tab_bar_if_only_one_tab = false
    end

    window:set_config_overrides(overrides)
  end
)

return {
  mux_enable_ssh_agent = true,
}
