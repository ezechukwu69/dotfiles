-- local function bootstrap(url, ref)
--   local name = url:gsub(".*/", "")
--   local path = vim.fn.stdpath [[data]] .. "/lazy/" .. name
--
--   if vim.fn.isdirectory(path) == 0 then
--     print(name .. ": installing in data dir...")
--
--     vim.fn.system { "git", "clone", url, path }
--     if ref then
--       vim.fn.system { "git", "-C", path, "checkout", ref }
--     end
--
--     vim.cmd [[redraw]]
--     print(name .. ": finished installing")
--   end
--   vim.opt.runtimepath:prepend(path)
-- end
--
-- bootstrap("https://github.com/udayvir-singh/tangerine.nvim")
--
-- -- Optional and only needed if you also want the macros
-- bootstrap("https://github.com/udayvir-singh/hibiscus.nvim")


require "load"
