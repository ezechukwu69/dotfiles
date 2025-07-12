local M = {
  opts = {
    width = 0.4,
    height = 0.9,
    float_buf = nil,
    float_win = nil,
    terminal_id = nil,
    set_default_keymaps = true,
  },
  command = "opencode",
}

local terminal = require("config.terminal")

function M.setup(opts)
  M.opts = vim.tbl_deep_extend("force", M.opts, opts or {})

  if M.opts.set_default_keymaps then
    vim.keymap.set("n", "<leader>ai", function()
      if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
        -- vim.api.nvim_win_close(M.opts.float_win, true)
        vim.api.nvim_win_hide(M.opts.float_win)
      else
        terminal.open_terminal(M, M.command, { width = 0.9, height = 0.8 })
      end
    end, { desc = "Open code" })

    vim.keymap.set("n", "<M-q>", function()
      if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
        -- vim.api.nvim_win_close(M.opts.float_win, true)
        vim.api.nvim_win_hide(M.opts.float_win)
      else
        terminal.open_terminal(M, M.command, { width = 0.9, height = 0.8 })
      end
    end, { desc = "Gemini" })
  end
end

return M
