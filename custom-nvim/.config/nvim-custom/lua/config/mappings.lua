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

-- map.set("n", "-", "<cmd>Explore<cr>", { noremap = true, silent = true })
