require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")
map("i", "<Esc>", "<ESC>", { desc = "Escape" })
map("n", "<leader>cf", function()
  vim.lsp.buf.format()
end, { desc = "Code Format" })
map("n", "<leader>tf", function()
  vim.g.autoformat_enabled = not vim.g.autoformat_enabled
  vim.notify("Autoformat toggled, current state is " .. tostring(vim.g.autoformat_enabled))
end, { desc = "Toggle Format" })

local del = vim.keymap.del

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")
