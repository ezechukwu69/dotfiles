vim.api.nvim_create_user_command('LspRestart', function()
  local bufnr = vim.api.nvim_get_current_buf()
  for _, client in pairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    client.stop(client, true)
  end
  vim.defer_fn(function()
    vim.cmd('edit')
  end, 100)
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


local utils = require("utils.pm")
local serialize = utils.serialize
local write_to_file = utils.write_to_file
local dofile_add = utils.dofile_add

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
