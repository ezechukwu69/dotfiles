-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
--
local dap = require("dap")

-- Toggle exception breakpoint
local function toggle_exception_breakpoint()
  local breakpoints = dap.list_breakpoints()
  local has_exception_breakpoint = false

  -- Check if any exception breakpoints exist
  for _, bp in ipairs(breakpoints) do
    if bp.condition and bp.condition:match("exception") then
      has_exception_breakpoint = true
      break
    end
  end

  if has_exception_breakpoint then
    -- Remove exception breakpoint
    print("Removing exception breakpoints")
    for _, bp in ipairs(breakpoints) do
      if bp.condition and bp.condition:match("exception") then
        dap.remove_breakpoint(bp.file, bp.line)
      end
    end
  else
    -- Add an exception breakpoint
    print("Adding exception breakpoint")
    dap.set_exception_breakpoints({ "raised", "uncaught" }) -- 'all' can be replaced with specific exception types.
  end
end

vim.keymap.set("n", "<leader>dE", function()
  toggle_exception_breakpoint()
end, { desc = "Toggle exception breakpoint" })

vim.keymap.set("n", "gS", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", { desc = "Dynamic workspace symbols" })
vim.keymap.set("n", "gs", "<cmd>FzfLua lsp_document_symbols<cr>", { desc = "LSP document symbols" })
