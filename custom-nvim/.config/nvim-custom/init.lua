_G.nvim_start_time = vim.loop.hrtime()
vim.o.packpath = vim.o.packpath .. "," .. vim.fn.stdpath("data") .. "," .. vim.fn.stdpath("config")

require('vim._extui').enable({
  -- target = "cmd"
  target = "box"
})

require("config.options")
require("config.usercommands")
require("config.mappings")
require("config.autocmds")
require("config.lsp")
require("plugins.init")
require("config.jj").setup()
require("config.gemini").setup()
require("config.opencode").setup()

if vim.g.neovide then
  vim.g.neovide_cursor_vfx_mode = "railgun"
  -- vim.o.guifont = "Aporetic Serif Mono:h11:b"
end

vim.cmd("colorscheme rose-pine")

local comment = vim.api.nvim_get_hl(0, { name = "Comment", link = false })
local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
local keyword = vim.api.nvim_get_hl(0, { name = '@keyword', link = true })

-- blink cmp
vim.api.nvim_set_hl(0, "BlinkCmpMenu", { bg = comment.bg, fg = normal.bg })
vim.api.nvim_set_hl(0, "BlinkCmpMenuBorder", { bg = "none", fg = comment.fg })
vim.api.nvim_set_hl(0, "BlinkCmpMenuSelection", { bg = comment.fg, fg = comment.bg })
