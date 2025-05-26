-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
-- vim.api.nvim_create_autocmd("BufEnter", {
--   group = vim.api.nvim_create_augroup("snacks.dim", {
--     clear = true,
--   }),
--   callback = function(opts)
--     Snacks.dim()
--   end,
-- })
--

vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
  group = vim.api.nvim_create_augroup("codelens_attach", {
    clear = true,
  }),
  callback = function(event)
    if vim.lsp.get_clients({ bufnr = event.buf }) ~= nil then
      vim.lsp.codelens.refresh()
    end
  end,
})

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp_attach", {
    clear = true,
  }),
  callback = function(event)
    vim.diagnostic.config({ virtual_lines = {
      current_line = true,
    } })
  end,
})
