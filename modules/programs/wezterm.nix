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
    package = config.lib.nixGL.wrap pkgs.unstable.wezterm;

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

        local solid_right_arrow = wezterm.nerdfonts.pl_left_hard_divider
        local solid_left_arrow = wezterm.nerdfonts.pl_right_hard_divider

        config.use_fancy_tab_bar = false
        config.tab_max_width = 22

        local tab_colors = {
          active_bg = 'black',
          active_fg = 'white',
          border_bg = 'black',
          inactive_bg = 'black',
          inactive_fg = 'white',
          dimmed_bg = 'black',
          dimmed_fg = 'white',
          remote_bg = 'black',
          remote_fg = 'white',
        }

        local hm_colors_ok, hm_colors = pcall(require, '../hm_colors')
        if hm_colors_ok then
          for k, v in pairs(hm_colors.tab_colors) do
            tab_colors[k] = v
          end
        end

        local function tab_title(tab_info)
          local title = tab_info.tab_title
          if title and #title > 0 then
            return title
          end

          title = tab_info.active_pane.title
          if title and #title > 0 then
            return title
          end

          return "shell"
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

            local current_hl_bg = current_bg:lighten(0.15)
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

        local function right_status(parts, text, extra_text, omit_right, bg, fg)
          bg = wezterm.color.parse(bg or tab_colors.inactive_bg)
          fg = wezterm.color.parse(fg or tab_colors.inactive_fg)

          table.insert(parts, { Background = { Color = tab_colors.border_bg } })

          if extra_text and #extra_text > 0 then
            local hl_bg = bg:lighten(0.15)
            local hl_fg = fg

            table.insert(parts, { Foreground = { Color = hl_bg } })
            table.insert(parts, { Text = solid_left_arrow })
            table.insert(parts, { Background = { Color = hl_bg } })
            table.insert(parts, { Foreground = { Color = hl_fg } })
            table.insert(parts, { Text = ' ' .. extra_text .. ' ' })
            table.insert(parts, { Background = { Color = hl_bg } })
            table.insert(parts, { Foreground = { Color = bg } })
            table.insert(parts, { Text = solid_left_arrow })
          else
            table.insert(parts, { Foreground = { Color = bg } })
            table.insert(parts, { Text = solid_left_arrow })
          end

          table.insert(parts, { Background = { Color = bg } })
          table.insert(parts, { Foreground = { Color = fg } })
          table.insert(parts, { Text = ' ' .. text .. ' ' })

          if omit_right == nil or not omit_right then
            table.insert(parts, { Background = { Color = bg } })
            table.insert(parts, { Foreground = { Color = tab_colors.border_bg } })
            table.insert(parts, { Text = solid_left_arrow })
          end
        end

        wezterm.on(
          'update-status',
          function(window, pane)
            local parts = {}
            local domain = pane:get_domain_name()
            local overrides = window:get_config_overrides() or {}

            if domain == 'local' or domain == 'default' then
              right_status(parts, wezterm.hostname())
            else
              local meta = pane:get_metadata() or {}
              local last_response_text = "";

              if meta.is_tardy then
                local last_response = meta.since_last_response_ms or 0

                last_response_text = last_response .. 'ms'
                if last_response > 60000 then
                  last_response_text = string.format("%.1fm", last_response / 60000)
                elseif last_response and last_response > 1000 then
                  last_response_text = string.format("%.1fs", last_response / 1000)
                end
              end

              if last_response_text and #last_response_text > 0 then
                last_response_text = ' ' .. last_response_text
              end

              right_status(
                parts,
                domain,
                last_response_text,
                false,
                tab_colors.remote_bg,
                tab_colors.remote_fg
              )
            end

            right_status(parts, wezterm.strftime("%H:%M"), nil, true)
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
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      terminal = "${weztermLauncher}";
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${weztermLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${weztermLauncher}"
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
