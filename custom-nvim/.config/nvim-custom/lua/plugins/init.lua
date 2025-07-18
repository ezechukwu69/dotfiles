local modules = require("utils.plugins")

local M = {}

local utils = require("utils.pm")

local plugins = utils.plugins
local configs = utils.configs
local inits = utils.inits
local opts = utils.opts
local builds = utils.builds
local is_array = utils.is_array

local start = utils.start


M.start = start


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

for _, opt in ipairs(opts) do
  opt()
end

for _, build in ipairs(builds) do
  build()
end

for _, init in ipairs(inits) do
  init()
end

for _, config in ipairs(configs) do
  config()
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
