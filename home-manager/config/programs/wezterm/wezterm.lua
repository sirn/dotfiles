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
