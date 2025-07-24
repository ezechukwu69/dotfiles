vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.lsp.set_log_level("WARN")
vim.env.PATH = vim.env.HOME .. "/.local/share/mise/shims:" .. vim.env.PATH
-- DIAGNOSTICS

-- How diagnostics are displayed
vim.diagnostic.config({
  virtual_lines = { current_line = true },
  update_in_insert = false,
  float = {
    border = "rounded",
    source = true,
  },
  severity_sort = true,
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = " ",
      [vim.diagnostic.severity.WARN] = " ",
      [vim.diagnostic.severity.INFO] = " ",
      [vim.diagnostic.severity.HINT] = " ",
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = "ErrorMsg",
      [vim.diagnostic.severity.WARN] = "WarningMsg",
    },
  }
  -- virtual_lines = true,
  -- virtual_text = {
  --   current_line = true,
  -- },
})

vim.lsp.inlay_hint.enable(true);

vim.cmd [[
  set complete=o,.,w,b,u
]]

local _o = vim.o;
local opt = vim.opt;

-- Basic Settings
_o.number = true;
_o.relativenumber = true;
_o.cursorline = false;
_o.numberwidth = 4;   -- minimal number of columns to use for the
_o.scrolloff = 10     -- scrolloff
_o.sidescrolloff = 10 -- sidescrolloff
_o.wrap = false;      -- disable line wrapping
_o.spell = false;     -- disable spell check
_o.list = true;       -- hide whitespace
-- _o.listchars = "tab:▸ ,trail:▸";
_o.listchars = "tab:▸ ";
_o.showbreak = "↳  ";
_o.splitbelow = true;   -- vertical splits
_o.splitright = true;   -- horizontal splits
-- indentation
_o.tabstop = 2;         -- spaces per tab
_o.shiftwidth = 2;      -- spaces per tab
_o.softtabstop = 2;     -- spaces when tab
_o.expandtab = true     -- convert tabs to spaces
_o.autoindent = true    -- copy indent from current line
_o.smartindent = true   -- smart auto indent
-- search settings
_o.smartcase = true     -- case sensitive by default if uppercase
_o.ignorecase = true    -- ignore case in search patterns
_o.hlsearch = true      -- highlight all matches on previous search pattern
_o.incsearch = true     -- show matches as you type
-- -- visual
_o.termguicolors = true -- enable true color
_o.colorcolumn = "100"  -- column to show vertical separator
_o.signcolumn = "yes"   -- always show signcolumns
_o.showmatch = true     -- show matching brackets
_o.matchtime = 2        -- highlight matching pairs after n seconds
_o.cmdheight = 0        -- height of the command bar
_o.pumheight = 10       -- pop up menu height
_o.pumblend = 10        -- popup menu transparency
_o.winblend = 0         -- window transparency
-- _o.conceallevel = 0     -- don't hide markup
-- -- _o.concealcursor = ""   -- don't hide cursorline markup
_o.lazyredraw = true -- don't redraw while executing macros (for speed)
_o.synmaxcol = 300   -- don't syntax highlight long lines
-- -- _o.wildmenu = true                -- enable wildmenu
-- -- _o.wildmode = "longest,list,full" -- wildmenu mode
opt.winborder = "rounded"
opt.diffopt:append("linematch:60")
_o.redrawtime = 10000
_o.maxmempattern = 20000
_o.showtabline = 1
_o.tabline = ""
--
-- -- File handling
_o.backup = false                            -- creates a backup file
_o.writebackup = false                       -- in case of a crash
_o.swapfile = false                          -- creates a swapfile
_o.undofile = true                           -- enable persistent undo
_o.undodir = vim.fn.expand("~/.vim/undodir") -- set undo directory
_o.updatetime = 300;                         -- time to update neovim in ms
_o.timeoutlen = 500                          -- key timeout duration
_o.ttimeoutlen = 0                           -- time to wait for a key code sequence
_o.autoread = true                           -- auto read file when changed on disk
_o.autowrite = false                         -- auto write file when changed on disk
--
-- -- Behaviour settings
_o.hidden = true                  -- enable hidden buffers
_o.errorbells = false             -- no error bells
_o.backspace = "indent,eol,start" -- better backspace behaviour
_o.autochdir = false              -- don't change dir automatically
opt.iskeyword:append("-");        -- treat words with '-' as part of words
opt.path:append("**")             -- recursively search for file in path
-- _o.selection = "exclusive"
_o.modifiable = true
_o.encoding = "UTF-8"
opt.clipboard:append("unnamedplus");
opt.laststatus = 3

local undodir = vim.fn.expand("~/.vim/undodir")
if vim.fn.isdirectory(undodir) == 0 then
  vim.fn.mkdir(undodir, "p")
end
