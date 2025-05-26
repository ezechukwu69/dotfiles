-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.g.maplocalleader = "\\"
-- vim.g.snacks_dim = true
vim.g.lazyvim_picker = "snacks"

vim.o.completeopt = "menu,noinsert,noselect,popup"

vim.cmd([[
highlight DiffAdd    guibg=#003300 guifg=NONE ctermbg=22 ctermfg=NONE
highlight DiffChange guibg=#333300 guifg=NONE ctermbg=94 ctermfg=NONE
highlight DiffDelete guibg=#330000 guifg=#ff0000 ctermbg=52 ctermfg=196
highlight DiffText   guibg=#444400 guifg=NONE ctermbg=58 ctermfg=NONE
set diffopt+=iwhite    " Ignore whitespace
set diffopt+=algorithm:patience  " Better diff algorithm
set diffopt+=vertical
]])
