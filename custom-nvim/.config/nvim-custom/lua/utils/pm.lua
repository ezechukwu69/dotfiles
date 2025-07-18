local M = {}

function M.setKeymaps(keymap)
  local key = keymap[1]
  local command = keymap[2]
  local desc = keymap.desc
  local modes = keymap.modes
  vim.keymap.set(modes or "n", key, command, { desc = desc or "" })
end

M.plugins = {}
M.inits = {}
M.configs = {}
M.builds = {}
M.opts = {}


local command_group = vim.api.nvim_create_augroup("Plugins2", { clear = true })
M.group = vim.api.nvim_create_augroup("Plugins2AutoCommands", { clear = true })

function M.is_array(t)
  if type(t) ~= "table" then return false end

  local count = 0
  for k, _ in pairs(t) do
    if type(k) ~= "number" then return false end
    count = count + 1
  end

  return count == #t
end

function M.get_name_full(path)
  local parts = vim.split(path, "/")
  local name = parts[#parts]
  return name
end

function M.addGitHub(path)
  local src = path
  if not src:match("^https://github.com") then
    src = "https://github.com/" .. src
  end
  return src
end

function M.start(module)
  local src = module[1]
  -- check if it starts with https://github.com and if not add it
  src = M.addGitHub(src)



  local plugin = {
    src = src,
  }
  -- name = module.name,
  -- version = module.version
  if module.name then
    plugin.name = module.name
  end
  if module.version then
    plugin.version = module.version
  end
  if module.event then
    vim.api.nvim_create_autocmd(module.event, {
      group = M.group,
      once = true,
      callback = function(args)
        if module.dependencies then
          if type(module.dependencies) == "string" then
            table.insert(M.plugins, M.addGitHub(module.dependencies))
          else
            for _, nested_module in ipairs(module.dependencies) do
              if type(nested_module) == "string" then
                table.insert(M.plugins, M.addGitHub(nested_module))
              else
                M.start(nested_module)
              end
            end
          end
        end
        vim.cmd.packadd(M.get_name_full(module[1]))
        if module.init and type(module.init) == "function" then
          module.init()
        end
        if module.config and type(module.config) == "function" then
          module.config()
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

          local setup = require(part).setup
          if setup then
            setup(opt)
          end
        end
        if module.keys then
          for _, keymap in ipairs(module.keys) do
            M.setKeymaps(keymap)
          end
        end
      end
    })
  elseif module.ft then
    vim.api.nvim_create_autocmd("FileType", {
      group = M.group,
      pattern = module.ft,
      once = true,
      callback = function(args)
        if module.dependencies then
          if type(module.dependencies) == "string" then
            table.insert(M.plugins, M.addGitHub(module.dependencies))
          else
            for _, nested_module in ipairs(module.dependencies) do
              if type(nested_module) == "string" then
                table.insert(M.plugins, M.addGitHub(nested_module))
              else
                M.start(nested_module)
              end
            end
          end
        end
        vim.cmd.packadd(M.get_name_full(module[1]))
        if module.init and type(module.init) == "function" then
          module.init()
        end
        if module.config and type(module.config) == "function" then
          module.config()
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

          local setup = require(part).setup
          if setup then
            setup(opt)
          end
        end
        if module.keys then
          for _, keymap in ipairs(module.keys) do
            M.setKeymaps(keymap)
          end
        end
      end
    })
  else
    if module.dependencies then
      if type(module.dependencies) == "string" then
        table.insert(M.plugins, M.addGitHub(module.dependencies))
      else
        for _, nested_module in ipairs(module.dependencies) do
          if type(nested_module) == "string" then
            table.insert(M.plugins, M.addGitHub(nested_module))
          else
            M.start(nested_module)
          end
        end
      end
    end
    table.insert(M.plugins, plugin)
    if module.init and type(module.init) == "function" then
      table.insert(M.inits, module.init)
    end
    if module.config and type(module.config) == "function" then
      table.insert(M.configs, module.config)
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
      table.insert(M.opts, function() require(part).setup(opt) end)
    end
    if module.keys then
      for _, keymap in ipairs(module.keys) do
        M.setKeymaps(keymap)
      end
    end
  end
  if module.build then
    table.insert(M.builds, function()
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
          vim.notify(ev.data.spec.name .. ": " .. ev.data.kind)
          if ev.data.kind == "install" or ev.data.kind == "update" then
            vim.fn.system("cd " .. ev.data.path .. " && " .. command)
          end
        end
      })
    end)
  end
end

local function serialize(tbl)
  local lines = { "return {" }
  table.insert(lines, "\"" .. tbl[1] .. "\",")
  if tbl.opts then
    table.insert(lines, "opts = {}")
  end
  table.insert(lines, "}")
  return table.concat(lines, "\n")
end

local function write_to_file(path, content, on_success)
  local uv = vim.loop
  local fd = uv.fs_open(path, "w", 438) -- 438 = octal 0666 permission
  if fd then
    uv.fs_write(fd, content, -1)
    uv.fs_close(fd)
    if on_success then
      on_success()
    end
  else
    vim.notify("Failed to create file")
  end
end


local function serializer(tbl, indent)
  indent = indent or 0
  local lines = {}
  local pad = string.rep("  ", indent)

  table.insert(lines, pad .. "return {")

  for k, v in pairs(tbl) do
    local key
    if type(k) == "string" and k:match("^%a[%w_]*$") then
      key = k .. " = "
    else
      key = ""
    end

    local value
    if type(v) == "string" then
      value = string.format("%q", v)
    elseif type(v) == "number" or type(v) == "boolean" then
      value = tostring(v)
    elseif type(v) == "table" then
      value = serializer(v, indent + 1)
    else
      value = "nil -- unsupported type: " .. type(v)
    end

    table.insert(lines, string.rep("  ", indent + 1) .. key .. value .. ",")
  end

  table.insert(lines, pad .. "}")
  return table.concat(lines, "\n")
end

local function convert_to_key_value(tbl)
  local _map = {}
  for _, v in ipairs(tbl) do
    _map[v] = v
  end
  return _map
end

local function convert_to_array(tbl)
  local _map = {}
  for _, v in pairs(tbl) do
    table.insert(_map, v)
  end
  return _map
end


local function dofile_add(plugin_name)
  local path = vim.fn.stdpath("config")
  local plugins_dir = path .. "/lua/utils"
  local luafile = plugins_dir .. "/plugins.lua"
  local data = dofile(luafile)
  write_to_file(plugins_dir .. "/.plugins.lua-bak", serializer(data))
  data = convert_to_key_value(data)
  local string = "plugins." .. plugin_name
  data[string] = string
  data = convert_to_array(data)
  data = serializer(data)
  write_to_file(luafile, data)
end

M.dofile_add = dofile_add
M.serializer = serializer
M.serialize = serialize
M.convert_to_key_value = convert_to_key_value
M.convert_to_array = convert_to_array

return M
