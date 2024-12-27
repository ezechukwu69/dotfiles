require "nvchad.mappings"
local filter = require "filter"

-- add yours here

local map = vim.keymap.set

-- map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspAttach", {}),
  callback = function()
    map("n", "<space>cd", function() vim.diagnostic.open_float(nil, { border = "rounded" }) end,
      { desc = "Open diagnostic float", remap = false })
    map("n", "cd", function() vim.lsp.buf.rename() end, { desc = "Rename symbol", remap = false })
    map("n", "g.", function() vim.lsp.buf.code_action() end, { desc = "Code Action", remap = false })
    map("n", "gd", function() vim.lsp.buf.definition() end, { desc = "Go to definition", remap = false })
    map("n", "gD", function() vim.lsp.buf.declaration() end, { desc = "Go to declaration", remap = false })
    map("n", "gy", function() vim.lsp.buf.type_definition() end, { desc = "Go to type definition", remap = false })
    map("n", "gI", function() vim.lsp.buf.implementation() end, { desc = "Go to implementation", remap = false })
    map("n", "gh", function() vim.lsp.buf.hover() end, { desc = "Show hover information", remap = false })
    map("n", "<leader>cf", function() vim.lsp.buf.format() end, { desc = "Format buffer", remap = false })
    map("n", "gs",
      function() require("telescope.builtin").lsp_document_symbols({ symbols = filter.get_kind_filter() }) end,
      { desc = "List symbols in current document", remap = false })
    map("n", "gS",
      function() require("telescope.builtin").lsp_dynamic_workspace_symbols({ symbols = filter.get_kind_filter() }) end,
      { desc = "List symbols in workspace", remap = false })
  end,
})
