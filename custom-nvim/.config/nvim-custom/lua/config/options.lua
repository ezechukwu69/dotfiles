vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
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

-- Basic Settings
o.number = true;
o.relativenumber = true;
o.cursorline = true;
o.numberwidth = 4;   -- minimal number of columns to use for the
o.scrolloff = 10     -- scrolloff
o.sidescrolloff = 10 -- sidescrolloff
o.wrap = false;      -- disable line wrapping
o.spell = false;     -- disable spell check
o.list = true;       -- hide whitespace
-- o.listchars = "tab:▸ ,trail:▸";
o.listchars = "tab:▸ ";
o.showbreak = "↳  ";
o.splitbelow = true;   -- vertical splits
o.splitright = true;   -- horizontal splits
-- indentation
o.tabstop = 2;         -- spaces per tab
o.shiftwidth = 2;      -- spaces per tab
o.softtabstop = 2;     -- spaces when tab
o.expandtab = true     -- convert tabs to spaces
o.autoindent = true    -- copy indent from current line
o.smartindent = true   -- smart auto indent
-- search settings
o.smartcase = true     -- case sensitive by default if uppercase
o.ignorecase = true    -- ignore case in search patterns
o.hlsearch = true      -- highlight all matches on previous search pattern
o.incsearch = true     -- show matches as you type
-- visual
o.termguicolors = true -- enable true color
o.colorcolumn = "100"  -- column to show vertical separator
o.signcolumn = "yes"   -- always show signcolumns
o.showmatch = true     -- show matching brackets
o.matchtime = 2        -- highlight matching pairs after n seconds
o.cmdheight = 0        -- height of the command bar
o.completeopt = "menu,menuone,noselect"
o.pumheight = 10       -- pop up menu height
o.pumblend = 10        -- popup menu transparency
o.winblend = 0         -- window transparency
o.conceallevel = 0     -- don't hide markup
-- o.concealcursor = ""             -- don't hide cursorline markup
o.lazyredraw = true    -- don't redraw while executing macros (for speed)
o.synmaxcol = 300      -- don't syntax highlight long lines
-- o.wildmenu = true                -- enable wildmenu
-- o.wildmode = "longest,list,full" -- wildmenu mode
opt.winborder = "rounded"
opt.diffopt:append("linematch:60")
o.redrawtime = 10000
o.maxmempattern = 20000
o.showtabline = 1
o.tabline = ""

-- File handling
o.backup = false                            -- creates a backup file
o.writebackup = false                       -- in case of a crash
o.swapfile = false                          -- creates a swapfile
o.undofile = true                           -- enable persistent undo
o.undodir = vim.fn.expand("~/.vim/undodir") -- set undo directory
o.updatetime = 300;                         -- time to update neovim in ms
o.timeoutlen = 500                          -- key timeout duration
o.ttimeoutlen = 0                           -- time to wait for a key code sequence
o.autoread = true                           -- auto read file when changed on disk
o.autowrite = false                         -- auto write file when changed on disk

-- Behaviour settings
o.hidden = true                  -- enable hidden buffers
o.errorbells = false             -- no error bells
o.backspace = "indent,eol,start" -- better backspace behaviour
o.autochdir = false              -- don't change dir automatically
opt.iskeyword:append("-");       -- treat words with '-' as part of words
opt.path:append("**")            -- recursively search for file in path
o.selection = "exclusive"
o.modifiable = true
o.encoding = "UTF-8"
opt.clipboard:append("unnamedplus");
opt.laststatus = 3


local undodir = vim.fn.expand("~/.vim/undodir")
if vim.fn.isdirectory(undodir) == 0 then
  vim.fn.mkdir(undodir, "p")
end
