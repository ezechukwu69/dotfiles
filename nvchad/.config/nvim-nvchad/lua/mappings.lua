require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

-- map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")

map("n", "<space>cd", function() vim.diagnostic.open_float(nil, { border = "rounded" }) end,
  { desc = "Diagnostic float", remap = false })
