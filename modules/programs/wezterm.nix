{ config, lib, pkgs, ... }:

let
  cfg = config.programs.wezterm;

  fishcfg = config.programs.fish;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;
in
{
  programs.wezterm = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.unstable.wezterm;

    extraConfig = ''
      local act = wezterm.action
      local config = wezterm.config_builder()
      local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil
      local is_linux <const> = wezterm.target_triple:find("linux") ~= nil

      local shell = "${config.machine.interactiveShell}"
      local font = wezterm.font_with_fallback({
        'PragmataPro Mono Liga',
        'Source Han Code JP',
      })

      config.color_scheme = 'default'
      config.enable_scroll_bar = true
      config.font = font
      config.command_palette_font = font
      config.pane_select_font = font
      config.char_select_font = font

      config.freetype_load_target = 'Light'
      config.freetype_load_flags = 'NO_HINTING'
      config.hide_tab_bar_if_only_one_tab = true
      config.use_ime = true
      config.mux_enable_ssh_agent = true
      config.warn_about_missing_glyphs = false

      config.keys = {
        { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
      }

      if is_linux then
        config.window_padding = {
          left = '0.5cell',
          right = '1cell',
          top = '0',
          bottom = '0',
        }

        config.default_prog = { shell };
        config.font_size = 12.0
        config.command_palette_font_size = 12.0
        config.pane_select_font_size = 12.0
        config.char_select_font_size = 12.0
      end

      if is_darwin then
        config.window_decorations = "RESIZE"
        config.window_padding = {
          left = '1cell',
          right = '0.5cell',
          top = '0.5cell',
          bottom = '0',
        }

        config.default_prog = { shell, "--login" }
        config.font_size = 14.0;
        config.command_palette_font_size = 14.0
        config.pane_select_font_size = 14.0
        config.char_select_font_size = 14.0
      end

      config.use_fancy_tab_bar = false
      config.tab_max_width = 22

      local solid_right_arrow = wezterm.nerdfonts.pl_left_hard_divider
      local solid_left_arrow = wezterm.nerdfonts.pl_right_hard_divider

      local tab_colors = {
        active_bg = 'black',
        active_fg = 'white',
        border_bg = 'black',
        inactive_bg = 'black',
        inactive_fg = 'white',
        dimmed_bg = 'black',
        dimmed_fg = 'white',
      }

      local hm_ssh_ok, hm_ssh = pcall(require, 'hm_ssh')
      if hm_ssh_ok then
        config.default_ssh_auth_sock = hm_ssh.ssh_auth_sock
      end

      local hm_colors_ok, hm_colors = pcall(require, 'hm_colors')
      if hm_colors_ok then
        config.colors = {
          tab_bar = hm_colors.tab_bar,
        }
        for k, v in pairs(hm_colors.tab_colors) do
          tab_colors[k] = v
        end
      end

      function tab_title(tab_info)
        local title = tab_info.tab_title
        if title and #title > 0 then
          return title
        end
        return tab_info.active_pane.title
      end

      wezterm.on(
        'format-tab-title',
        function(tab, tabs, panes, config, hover, max_width)
          -- Get the current active tab index to style the wedge prior to active
          local active_tab_index = -1
          for i, t in ipairs(tabs) do
            if t.is_active then
              active_tab_index = t.tab_index
              break
            end
          end

          local current_bg = wezterm.color.parse(tab_colors.inactive_bg)
          local current_fg = wezterm.color.parse(tab_colors.inactive_fg)
          if tab.is_active then
            current_bg = wezterm.color.parse(tab_colors.active_bg)
            current_fg = wezterm.color.parse(tab_colors.active_fg)
          end

          local current_hl_bg = current_bg:lighten(0.1)
          local current_hl_fg = current_fg

          local parts = {}
          local trunc_right = 7

          table.insert(parts, { Background = { Color = current_hl_bg } })
          table.insert(parts, { Foreground = { Color = tab_colors.border_bg } })
          if tab.tab_index > 0 then
            table.insert(parts, { Text = solid_right_arrow })
            trunc_right = trunc_right + 1
          end

          table.insert(parts, { Foreground = { Color = current_hl_fg } })
          table.insert(parts, { Background = { Color = current_hl_bg } })
          table.insert(parts, { Text = ' ' .. (tab.tab_index + 1) .. ' ' })
          table.insert(parts, { Background = { Color = current_bg } })
          table.insert(parts, { Foreground = { Color = current_hl_bg } })
          table.insert(parts, { Text = solid_right_arrow })

          local title = tab_title(tab)
          title = wezterm.truncate_right(title, max_width - trunc_right)

          table.insert(parts, { Background = { Color = current_bg } })
          table.insert(parts, { Foreground = { Color = current_fg } })
          table.insert(parts, { Text = ' ' .. title .. ' ' })
          table.insert(parts, { Background = { Color = tab_colors.border_bg } })
          table.insert(parts, { Foreground = { Color = current_bg } })
          table.insert(parts, { Text = solid_right_arrow })

          return parts
        end
      )

      function status_section(parts, text, omit_right)
        table.insert(parts, { Background = { Color = tab_colors.border_bg } })
        table.insert(parts, { Foreground = { Color = tab_colors.dimmed_bg } })
        table.insert(parts, { Text = solid_left_arrow })
        table.insert(parts, { Background = { Color = tab_colors.dimmed_bg } })
        table.insert(parts, { Foreground = { Color = tab_colors.dimmed_fg } })
        table.insert(parts, { Text = ' ' .. text .. ' ' })

        if omit_right == nil or not omit_right then
          table.insert(parts, { Background = { Color = tab_colors.dimmed_bg } })
          table.insert(parts, { Foreground = { Color = tab_colors.border_bg } })
          table.insert(parts, { Text = solid_left_arrow })
        end
      end

      wezterm.on(
        'update-status',
        function(window, pane)
          local parts = {}
          local domain = pane:get_domain_name()

          if domain == 'local' or domain == 'default' then
            status_section(parts, wezterm.hostname())
          end

          status_section(parts, wezterm.strftime("%H:%M"), true)
          window:set_right_status(wezterm.format(parts))
        end
      )

      return config
    '';
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = lib.getExe cfg.package;
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${lib.getExe cfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${lib.getExe cfg.package}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf fuzzelcfg.enable {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
