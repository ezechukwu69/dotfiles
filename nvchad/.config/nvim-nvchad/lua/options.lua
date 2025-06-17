require "nvchad.options"

-- add yours here!

local o = vim.o
-- views can only be fully collapsed with the global statusline
vim.opt.laststatus = 3
-- Default splitting will cause your main splits to jump when opening an edgebar.
-- To prevent this, set `splitkeep` to either `screen` or `topline`.
vim.opt.splitkeep = "screen"
o.cursorlineopt = 'both' -- to enable cursorline!

vim.cmd [[
  set relativenumber
]]
