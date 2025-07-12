local modules = require("plugins.plugins")

local command_group = vim.api.nvim_create_augroup("Plugins2", { clear = true })

local function setKeymaps(keymap)
  local key = keymap[1]
  local command = keymap[2]
  local desc = keymap.desc
  local modes = keymap.modes
  vim.keymap.set(modes or "n", key, command, { desc = desc or "" })
end

local plugins = {}
local inits = {}
local configs = {}
local builds = {}
local opts = {}

local function addGitHub(path)
  local src = path
  if not src:match("^https://github.com") then
    src = "https://github.com/" .. src
  end
  return src
end

local M = {}

local function start(module)
  local src = module[1]
  -- check if it starts with https://github.com and if not add it
  src = addGitHub(src)

  if module.dependencies then
    if type(module.dependencies) == "string" then
      table.insert(plugins, addGitHub(module.dependencies))
    else
      for _, nested_module in ipairs(module.dependencies) do
        if type(nested_module) == "string" then
          table.insert(plugins, addGitHub(nested_module))
        else
          start(nested_module)
        end
      end
    end
  end

  if module.keys then
    for _, keymap in ipairs(module.keys) do
      setKeymaps(keymap)
    end
  end

  local plugin = {
    src = src,
  }
  -- name = module.name,
  -- version = module.version
  if module.name then
    plugin.name = module.name
  end
  if module.version then
    plugin.version = vim.version.range(module.version)
  end
  table.insert(plugins, plugin)
  if module.init and type(module.init) == "function" then
    table.insert(inits, module.init)
  end
  if module.config and type(module.config) == "function" then
    table.insert(configs, module.config)
  end
  if module.opts then
    local parts = vim.split(module[1], "/")
    local part = parts[#parts]
    part = part:gsub("%.nvim$", "")
    -- vim.notify(vim.inspect(part))
    local opt = {}
    if module.opts and type(module.opts) == "table" then
      opt = module.opts
    elseif module.opts and type(module.opts) == "function" then
      opt = module.opts(nil, opt)
    end
    table.insert(opts, function() require(part).setup(opt) end)
  end
  if module.build then
    table.insert(builds, function()
      local command = ""
      if type(module.build) == "string" then
        command = module.build
      else
        command = module.build()
      end
      vim.api.nvim_create_autocmd("PackChanged", {
        group = command_group,
        callback = function(ev)
          -- kind: install or update or delete
          -- ev.data contains
          -- - kind
          -- - spec - plugin specs
          -- - path - path
          vim.notify(vim.inspect(ev))
          vim.notify(ev.data.spec.name .. ": " .. ev.data.kind)
          if ev.data.kind == "install" or ev.data.kind == "update" then
            vim.fn.system("cd " .. ev.data.path .. " && " .. command)
          end
        end
      })
    end)
  end
end

M.start = start

local function is_array(t)
  if type(t) ~= "table" then return false end

  local count = 0
  for k, _ in pairs(t) do
    if type(k) ~= "number" then return false end
    count = count + 1
  end

  return count == #t
end

for _, mode in ipairs(modules) do
  local module = require(mode)
  if (is_array(module)) then
    for _, mod in ipairs(module) do
      start(mod)
    end
  else
    start(module)
  end
end

vim.pack.add(plugins)

for _, init in ipairs(inits) do
  init()
end

for _, config in ipairs(configs) do
  config()
end

for _, opt in ipairs(opts) do
  opt()
end

for _, build in ipairs(builds) do
  build()
end



-- [[
--   plugins2 can be defined in the format
--   {
--     src = "plugin-name/url",
--     config = function() end,
--     init = function() end,
--     build = "" or function() end, -- should return a string
--   }
--
--   vim.pack.add accepts
--   {
--     src = "",
--     name = "",
--     version = "",
--   }
-- ]]

return M
