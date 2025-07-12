vim.api.nvim_create_user_command('LspRestart', function()
  for _, client in pairs(vim.lsp.get_clients()) do
    client.stop(client, true)
  end
  vim.cmd('edit')
end, {})

local function map(tbl, func)
  local result = {}
  for i, v in ipairs(tbl) do
    result[i] = func(v, i)
  end
  return result
end

vim.api.nvim_create_user_command('PackDelete', function()
  vim.ui.select(map(vim.pack.get(), function(x)
    return x.spec.name
  end), {
    prompt = "Select package to uninstall",
  }, function(item)
    vim.pack.del({ item })
  end)
end, {})


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
  local plugins_dir = path .. "/lua/plugins"
  local data = dofile(plugins_dir .. "/plugins.lua")
  write_to_file(plugins_dir .. "/.plugins.lua-bak", serializer(data))
  data = convert_to_key_value(data)
  local string = "plugins." .. plugin_name
  data[string] = string
  data = convert_to_array(data)
  data = serializer(data)
  write_to_file(plugins_dir .. "/plugins.lua", data)
end

vim.api.nvim_create_user_command('PackInstall', function(args)
  local package = vim.fn.input({ prompt = "Enter git url of package to install or {user}/{package} to install: " })
  if not package or package == "" or type(package) ~= "string" then
    return
  end
  local names = vim.split(package, "/")
  local name = names[#names]
  name = name:gsub("%.nvim$", "")
  local path = vim.fn.stdpath("config")
  local plugins_dir = path .. "/lua/plugins"
  local plugin_file = plugins_dir .. "/" .. name .. ".lua"
  local done = function(has_opts)
    local value = {
      package,
    }
    if has_opts then
      value.opts = true
    end

    write_to_file(plugin_file, serialize(value), function()
      vim.ui.select({ "yes", "no" }, { prompt = "Source now? " }, function(choice)
        if not choice or choice == "" then
          return
        end
        if choice == "yes" then
          require("plugins.init").start(require("plugins." .. name))
        end
        dofile_add(name)
      end)
    end)
  end
  vim.ui.select({ "yes", "no" }, { prompt = "Has opts for config?: " }, function(choice)
    if not choice or choice == "" then
      return
    end
    done(choice == "yes")
  end)
end, {
  nargs = "*"
})

vim.api.nvim_create_user_command("PackUpdate", function(args)
  vim.pack.update()
end, {})
