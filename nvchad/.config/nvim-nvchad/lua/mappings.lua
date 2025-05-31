require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")
map("i", "<Esc>", "<ESC>", { desc = "Escape"})
map("n", "<leader>cf", function ()
  vim.lsp.buf.format()
end, {desc = "Code Format"})

local del = vim.keymap.del

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
