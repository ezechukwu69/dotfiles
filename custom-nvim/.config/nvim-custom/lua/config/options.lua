vim.lsp.set_log_level("WARN")
-- DIAGNOSTICS

-- How diagnostics are displayed
vim.diagnostic.config({
    -- virtual_lines = { current_line = true }
    virtual_text = {
        current_line = true,
    },
})

local o = vim.o;
local opt = vim.opt;

o.number = true;
o.relativenumber = true;
o.numberwidth = 4;
o.signcolumn = "yes";
o.wrap = false;
o.spell = false;
o.list = false;
o.listchars = "tab:▸ ,trail:▸";
o.showbreak = "↳  ";
o.matchtime = 3;
o.undofile = true;
o.tabstop = 4;
o.shiftwidth = 4;
o.expandtab = true;
o.softtabstop = 4;
o.splitbelow = true;
o.splitright = true;
o.updatetime = 1000;
opt.clipboard:append("unnamedplus");
opt.ignorecase = true
opt.smartcase = true
opt.winborder = "rounded"
opt.laststatus = 3
opt.termguicolors = true
vim.cmd("colorscheme oxocarbon")
