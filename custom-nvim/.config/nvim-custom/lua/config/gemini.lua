local M = {
  opts = {
    width = 0.4,
    height = 0.9,
    float_buf = nil,
    float_win = nil,
    terminal_id = nil,
    set_default_keymaps = true,
  },
  command = "gemini --model gemini-2.5-pro",
}

function M.open_float_window_with_keymap()
  local calculate_columns = function(columns)
    local width = math.floor(columns * M.opts.width)
    return vim.o.columns - width
  end

  local opts = {
    relative = "editor",
    width = math.floor(vim.o.columns * M.opts.width),
    height = math.floor(vim.o.lines * M.opts.height),
    row = 0,
    col = calculate_columns(vim.o.columns),
    style = "minimal",
    border = "rounded",
  }

  if M.opts.float_buf and vim.api.nvim_buf_is_valid(M.opts.float_buf) then
    if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
      vim.api.nvim_set_current_win(M.opts.float_win)
      vim.cmd.startinsert()
      return M.opts.float_buf
    else
      M.opts.float_win = vim.api.nvim_open_win(M.opts.float_buf, true, opts)
      vim.cmd.startinsert()
      return M.opts.float_buf
    end
  end

  M.opts.float_buf = vim.api.nvim_create_buf(false, true)

  M.opts.float_win = vim.api.nvim_open_win(M.opts.float_buf, true, opts)

  vim.bo[M.opts.float_buf].filetype = "terminal"

  M.opts.terminal_id = vim.fn.jobstart(M.command, {
    term = true,
    on_exit = function(_, exit_code)
      if exit_code == 0 then
        vim.api.nvim_win_close(M.opts.float_win, true)
        vim.api.nvim_buf_delete(M.opts.float_buf, { force = true })
      end
    end,
  })
  vim.cmd.startinsert()

  vim.keymap.set("t", "<M-q>", function()
    vim.notify("closing gemini")
    if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
      -- vim.api.nvim_win_close(M.opts.float_win, true)
      vim.api.nvim_win_hide(M.opts.float_win)
    end
  end, { buffer = M.opts.float_buf, nowait = true, silent = true })

  return M.opts.float_buf
end

function M.setup(opts)
  M.opts = vim.tbl_deep_extend("force", M.opts, opts or {})

  if M.opts.set_default_keymaps then
    vim.keymap.set("n", "<leader>gm", function()
      if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
        -- vim.api.nvim_win_close(M.opts.float_win, true)
        vim.api.nvim_win_hide(M.opts.float_win)
      else
        M.open_float_window_with_keymap()
      end
    end, { desc = "Gemini" })

    vim.keymap.set("n", "<M-q>", function()
      if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
        -- vim.api.nvim_win_close(M.opts.float_win, true)
        vim.api.nvim_win_hide(M.opts.float_win)
      else
        M.open_float_window_with_keymap()
      end
    end, { desc = "Gemini" })
  end
end

return M
