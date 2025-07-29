local map = vim.keymap

map.set("n", "<leader><esc>", "<cmd>nohl<CR>", { noremap = true, silent = true })
map.set("n", "<leader>hrr", "<cmd>so %<cr>", { noremap = true, silent = true })
map.set("n", "<leader>F", ":find ", { noremap = true, silent = true })

map.set("n", "<leader>tn", ":tabnew<cr>", { noremap = true, silent = true, desc = "New Tab" })
map.set("n", "<leader>tx", ":tabclose<cr>", { noremap = true, silent = true, desc = "Close Tab" })

map.set("n", "<leader>tm", ":tabmove<cr>", { noremap = true, silent = true, desc = "Move Tab" })
map.set("n", "<leader>t>", ":tabmove +1<cr>", { noremap = true, silent = true, desc = "Move Tab Right" })
map.set("n", "<leader>t<", ":tabmove -1<cr>", { noremap = true, silent = true, desc = "Move Tab Left" })
map.set("n", "<leader>tl", ":tabnext<cr>", { noremap = true, silent = true, desc = "Next Tab" })
map.set("n", "<leader>th", ":tabprevious<cr>", { noremap = true, silent = true, desc = "Previous Tab" })

map.set("n", "grc", function() vim.lsp.buf.incoming_calls() end,
  { noremap = true, silent = true, desc = "Incoming Calls" })
map.set("n", "grC", function() vim.lsp.buf.outgoing_calls() end,
  { noremap = true, silent = true, desc = "Outgoing Calls" })
map.set("n", "<leader>id", function() vim.cmd.detach() end,
  { noremap = true, silent = true, desc = "Detach from Neovim" })

map.set("n", "<leader>ft", ":echo &filetype<cr>", { noremap = true, silent = true, desc = "Check filetype" })
map.set("n", "<C-x>0", "<C-w>q", { noremap = true, silent = true, desc = "Close current window" })
map.set("n", "<C-x>1", "<C-w>o", { noremap = true, silent = true, desc = "Delete other window" })
map.set("n", "<C-x>2", "<C-w>s", { noremap = true, silent = true, desc = "Split Below" })
map.set("n", "<C-x>3", "<C-w>v", { noremap = true, silent = true, desc = "Split Right" })
map.set("n", "<M-z>", "dt", { noremap = true, silent = true, desc = "Delete to character" })
map.set("n", "<C-x>o", "<C-w><C-w>", { noremap = true, silent = true, desc = "Other window" })
map.set("n", "<M-f>", "<C-o>e", { noremap = true, silent = true, desc = "Move word forward in insert" })
map.set("n", "<C-x>h", "GVgg", { noremap = true, silent = true, desc = "Move word forward in insert" })

-- Split bindings
map.set("n", "<C-x>4<C-o>", function()
  vim.cmd.vsplit()
  Snacks.picker.buffers()
end, { desc = "Open buffer in split" })

map.set("n", "<C-s>", "/", { noremap = true, silent = true, desc = "Search forward" })
-- map.set("n", "-", "<cmd>Explore<cr>", { noremap = true, silent = true })
