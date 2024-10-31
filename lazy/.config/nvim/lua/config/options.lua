-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
--remove nrtrw default heading and help menu

vim.opt.autochdir = true
vim.g.netrw_banner = 0 -- Disable the banner (help menu)
vim.g.netrw_liststyle = 0 -- Use a tree-style listing without headings
