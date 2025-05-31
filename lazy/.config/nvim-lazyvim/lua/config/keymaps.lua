-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
--

local map = vim.keymap.set

-- map("n", "gS", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", { desc = "Dynamic workspace symbols" })
-- map("n", "gs", "<cmd>FzfLua lsp_document_symbols<cr>", { desc = "LSP document symbols" })
-- map("n", "cd", function()
--   require("snacks.rename").rename()
-- end, { desc = "LSP document symbols" })
-- map("n", "<space>cr", function()
--   require("nvchad.lsp.renamer")()
-- end, { desc = "LSP document symbols" })

-- map("n", "<leader>dz", function()
--   require("dapui.elements.console").toggle({})
-- end, {
--   desc = "Toggle scopes"
-- })
--

map("n", "<leader>h", function() end, { desc = "terminal new horizontal term" })

map("n", "<leader>v", function()
  require("nvchad.term").new({ pos = "vsp" })
end, { desc = "terminal new vertical term" })

-- toggleable
map({ "n", "t" }, "<A-h>", function()
  Snacks.terminal()
end, { desc = "terminal toggleable horizontal term" })

map({ "n", "t" }, "<A-n>", function()
  Snacks.terminal.open()
end, { desc = "terminal toggle floating term" })

map("n", "<leader>pt", function()
  require("nvchad.themes").open()
end, { desc = "Pick theme" })

map({ "i" }, "jk", function()
  vim.cmd.stopinsert()
end, { desc = "Escape from insert", nowait = true })

map({ "i" }, "jj", function()
  vim.cmd.stopinsert()
end, { desc = "Escape from insert", nowait = true })

-- Diff Keymaps
vim.keymap.set("n", "]c", "<cmd>diffget!<CR>", { desc = "Diff Get" })
vim.keymap.set("n", "[c", "<cmd>diffput!<CR>", { desc = "Diff Put" })

-- Toggle exception breakpoint
-- local function toggle_exception_breakpoint()
--   local breakpoints = dap.list_breakpoints()
--   local has_exception_breakpoint = false
--
--   -- Check if any exception breakpoints exist
--   for _, bp in ipairs(breakpoints) do
--     if bp.condition and bp.condition:match("exception") then
--       has_exception_breakpoint = true
--       break
--     end
--   end
--
--   if has_exception_breakpoint then
--     -- Remove exception breakpoint
--     print("Removing exception breakpoints")
--     for _, bp in ipairs(breakpoints) do
--       if bp.condition and bp.condition:match("exception") then
--         dap.remove_breakpoint(bp.file, bp.line)
--       end
--     end
--   else
--     -- Add an exception breakpoint
--     print("Adding exception breakpoint")
--     dap.set_exception_breakpoints({ "raised", "uncaught" }) -- 'all' can be replaced with specific exception types.
--   end
-- end

-- vim.keymap.set("n", "<leader>dE", function()
--   toggle_exception_breakpoint()
-- end, { desc = "Toggle exception breakpoint" })
