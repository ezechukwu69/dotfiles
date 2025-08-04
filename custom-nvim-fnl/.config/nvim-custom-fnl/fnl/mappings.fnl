(import-macros {: map!} :macros)

(map!
  :mode "n" 
  :key "<leader><esc>"
  :command "<cmd>nohl<CR>"
  :options {:noremap true :silent true})

(map!
  :mode "n" 
  :key "<leader>hrr"
  :command "<cmd>so %<CR>"
  :options {:noremap true :silent true})

(map!
  :mode "n" :key "<leader>F"
  :command ":find "
  :options {:noremap true :silent true})

(map!
  :mode "n" :key "<leader>tn"
  :command ":tabnew<CR>"
  :options {:desc "New Tab" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>tx"
  :command ":tabclose<CR>"
  :options {:desc "Close Tab" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>tm"
  :command ":tabmove<CR>"
  :options {:desc "Move Tab" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>t>"
  :command ":tabmove +1<CR>"
  :options {:desc "Move Tab Right" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>t<"
  :command ":tabmove -1<CR>"
  :options {:desc "Move Tab Left" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>tl"
  :command ":tabnext<CR>"
  :options {:desc "Next Tab" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>th"
  :command ":tabprevious<CR>"
  :options {:desc "Previous Tab" :noremap true :silent true})

(map!
  :mode "n" 
  :key "grc"
  :command (fn [] (vim.lsp.buf.incoming_calls))
  :options {:desc "Incoming Calls" :noremap true :silent true})

(map!
  :mode "n" :key "grC"
  :command (fn [] (vim.lsp.buf.outgoing_calls))
  :options {:desc "Outgoing Calls" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>id"
  :command (fn [] (vim.cmd.detach))
  :options {:desc "Detach from Neovim" :noremap true :silent true})

(map!
  :mode "n" :key "<leader>ft"
  :command ":echo &filetype<CR>"
  :options {:desc "Check filetype" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>0"
  :command "<C-w>q"
  :options {:desc "Close current window" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>1"
  :command "<C-w>o"
  :options {:desc "Delete other window" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>2"
  :command "<C-w>s"
  :options {:desc "Split Below" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>3"
  :command "<C-w>v"
  :options {:desc "Split Right" :noremap true :silent true})

(map!
  :mode "n" :key "<M-z>"
  :command "dt"
  :options {:desc "Delete to character" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>o"
  :command "<C-w><C-w>"
  :options {:desc "Other window" :noremap true :silent true})

(map!
  :mode "n" :key "<M-f>"
  :command "<C-o>e"
  :options {:desc "Move word forward in insert" :noremap true :silent true})

(map!
  :mode "n" :key "<C-x>h"
  :command "GVgg"
  :options {:desc "Move word forward in insert" :noremap true :silent true})

(map!
  :mode "n" :key "<C-s>"
  :command "/"
  :options {:desc "Search forward" :noremap true :silent true})
