local map = vim.keymap

map.set("i", "<esc>", "<C-c>", { noremap = true, silent = true })
map.set("n", "<esc>", "<cmd>nohl<CR>", { noremap = true, silent = true })
map.set("n", "<leader>hrr", "<cmd>so %<cr>", { noremap = true, silent = true })
map.set("n", "-", "<cmd>Explore<cr>", { noremap = true, silent = true })
