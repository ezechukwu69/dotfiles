-- bootstrap lazy.nvim, LazyVim and your plugins
vim.g.base46_cache = vim.fn.stdpath("data") .. "/base46/"
require("config.lazy")
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

-- dofile(vim.fn.stdpath("config") .. "/lua/chadrc.lua")
