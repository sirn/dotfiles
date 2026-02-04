{ config, lib, pkgs, ... }:

let
  cfg = config.programs.wezterm;

  fishcfg = config.programs.fish;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  swaycfg = config.wayland.windowManager.sway;

  weztermLauncher = config.lib.machine.wrapLauncher cfg.package;
in
{
  programs.wezterm = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.wezterm;

    extraConfig = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()

      local function deep_merge(target, source)
        for k, v in pairs(source) do
          if type(v) == 'table' and type(target[k]) == 'table' then
            deep_merge(target[k], v)
          else
            target[k] = v
          end
        end
      end

      local function load_modules(dir_name)
        local module_path = wezterm.config_dir .. '/' .. dir_name
        local files = wezterm.read_dir(module_path)

        for _, file_path in ipairs(files) do
          local filename = file_path:match(".*/(.+)$")
          if filename and filename:match("%.lua$") and filename ~= 'init.lua' then
            local module_name = dir_name .. '.' .. filename:gsub('%.lua$', "")
            local ok, module_config = pcall(require, module_name)
            if ok and type(module_config) == 'table' then
              deep_merge(config, module_config)
            elseif not ok then
              wezterm.log_error("Error loading module '" .. module_name .. "': " .. module_config)
            end
          end
        end
      end

      load_modules('modules')
      return config
    '';
  };

  xdg.configFile = {
    "wezterm/modules/fonts.lua" = {
      text = ''
        local wezterm = require 'wezterm'
        local config = wezterm.config_builder()

        local font = wezterm.font_with_fallback({
          'PragmataPro Mono Liga',
          'Source Han Code JP',
        })

        config.font = font
        config.command_palette_font = font
        config.pane_select_font = font
        config.char_select_font = font
        config.warn_about_missing_glyphs = false

        ${if pkgs.stdenv.isDarwin then ''
          config.font_size = 14.0;
          config.command_palette_font_size = 14.0
          config.pane_select_font_size = 14.0
          config.char_select_font_size = 14.0
        '' else ''
          config.font_size = 12.0
          config.command_palette_font_size = 12.0
          config.pane_select_font_size = 12.0
          config.char_select_font_size = 12.0
        ''}

        config.use_ime = true
        config.freetype_load_target = 'Light'
        config.freetype_load_flags = 'NO_HINTING'

        return config
      '';
    };
    "wezterm/modules/shell.lua" = {
      text = ''
        return {
          default_prog = {
            "${config.machine.interactiveShell}",
            ${lib.optionalString pkgs.stdenv.isDarwin ''
              "--login",
            ''}
          }
        }
      '';
    };
    "wezterm/modules/tabbar.lua" = {
      text = ''
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
      '';
    };
    "wezterm/modules/mux.lua" = {
      text = ''
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
      '';
    };
    "wezterm/modules/window.lua" = {
      text = ''
        return {
          enable_scroll_bar = true,
          ${if pkgs.stdenv.isDarwin then ''
            window_decorations = "RESIZE",
            window_padding = {
              left = '1cell',
              right = '0.5cell',
              top = '0.5cell',
              bottom = '0',
            },
          '' else ''
            window_padding = {
              left = '0.5cell',
              right = '1cell',
              top = '0',
              bottom = '0',
            },
          ''}
        }
      '';
    };
    "wezterm/modules/keybindings.lua" = {
      text = ''
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
      '';
    };
    # Workaround for https://github.com/wezterm/wezterm/issues/6685
    # Clipboard not working between terminals on Wayland
    "wezterm/modules/workaround-6685.lua" = lib.mkIf (swaycfg.enable || niricfg.enable) {
      text = ''
        local wezterm = require 'wezterm'

        wezterm.on(
          'window-focus-changed',
          function(window, pane)
            wezterm.run_child_process {
              'sh', '-c',
              '${pkgs.wl-clipboard}/bin/wl-paste -n | ${pkgs.wl-clipboard}/bin/wl-copy'
            }
          end
        )

        return {}
      '';
    };
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      terminal = "${weztermLauncher}";
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${weztermLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${weztermLauncher}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf (cfg.enable && fuzzelcfg.enable) {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
