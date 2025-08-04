(set vim.g.mapleader " ")
(set vim.g.maplocalleader "\\")
(vim.lsp.set_log_level "WARN")

(local o vim.o)
(vim.cmd "inoremap <expr> <cr> pumvisible() ? '<c-y>' : '<cr>'")
(local opt vim.opt)

(set o.number true)
(set o.relativenumber true)

(set o.cursorline  false)
(set o.numberwidth  4)   ;; minimal number of columns to use for the
(set o.scrolloff  10)     ;; scrolloff
(set o.sidescrolloff  10) ;; sidescrolloff
(set o.wrap  false)      ;; disable line wrapping
(set o.spell  false)     ;; disable spell check
(set o.list  true)      ;; hide whitespace

(set o.listchars  "tab:▸ ")
(set o.showbreak  "↳  ")
(set o.splitbelow  true)   ;; vertical splits
(set o.splitright  true)   ;; horizontal splits
(set o.tabstop  2)         ;; spaces per tab
(set o.shiftwidth  2)      ;; spaces per tab
(set o.softtabstop  2)     ;; spaces when tab
(set o.expandtab  true)     ;; convert tabs to spaces
(set o.autoindent  true)    ;; copy indent from current line
(set o.smartindent  true)   ;; smart auto indent
(set o.smartcase  true)     ;; case sensitive by default if uppercase
(set o.ignorecase  true)    ;; ignore case in search patterns
(set o.hlsearch  true)      ;; highlight all matches on previous search pattern
(set o.incsearch  true)     ;; show matches as you type
(set o.termguicolors  true) ;; enable true color
(set o.colorcolumn  "100")  ;; column to show vertical separator
(set o.signcolumn  "yes")   ;; always show signcolumns
(set o.showmatch  true)     ;; show matching brackets
(set o.matchtime  2)        ;; highlight matching pairs after n seconds
(set o.cmdheight  0)        ;; height of the command bar
(set o.pumheight  10)       ;; pop up menu height
(set o.pumblend  10)        ;; popup menu transparency
(set o.winblend  0)         ;; window transparency
(set o.lazyredraw  true) ;; don't redraw while executing macros (for speed)
(set o.synmaxcol  300)   ;; don't syntax highlight long lines


(set opt.winborder  "rounded")
(opt.diffopt:append "linematch:60")
(set o.redrawtime  10000)
(set o.maxmempattern  20000)
(set o.showtabline  1)
(set o.tabline  "")

;; File handling
(set o.backup false)                            ;; creates a backup file
(set o.writebackup false)                      ;; in case of a crash
(set o.swapfile  false)                          ;; creates a swapfile
(set o.undofile  true)                           ;; enable persistent undo
(set o.undodir  (vim.fn.expand "~/.vim/undodir")) ;; set undo directory
(set o.updatetime  300)                         ;; time to update neovim in ms
(set o.timeoutlen  500)                          ;; key timeout duration
(set o.ttimeoutlen  0)                           ;; time to wait for a key code sequence
(set o.autoread  true)                           ;; auto read file when changed on disk
(set o.autowrite  false)                         ;; auto write file when changed on disk
;; ;; Behaviour settings
(set o.hidden  true)                  ;; enable hidden buffers
(set o.errorbells  false)             ;; no error bells
(set o.backspace  "indent,eol,start") ;; better backspace behaviour
(set o.autochdir  false)              ;; don't change dir automatically
(opt.iskeyword:append "-" )        ;; treat words with '-' as part of words
(opt.path:append "**")             ;; recursively search for file in path
(set o.modifiable  true)
(set o.encoding  "UTF-8")
(opt.clipboard:append "unnamedplus")
(set opt.laststatus  3)
