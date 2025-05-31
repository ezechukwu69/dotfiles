require "nvchad.options"

-- add yours here!

local o = vim.o
o.cursorlineopt ='both' -- to enable cursorline!

vim.opt.laststatus = 3
-- Default splitting will cause your main splits to jump when opening an edgebar.
-- To prevent this, set `splitkeep` to either `screen` or `topline`.
vim.opt.splitkeep = "screen"

vim.cmd[[set relativenumber]]
