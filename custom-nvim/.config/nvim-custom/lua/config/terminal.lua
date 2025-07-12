local M = {}

function M.open_terminal(O, command, override_opts)
  local ov_opts = vim.tbl_deep_extend("force", O.opts, override_opts or {})
  local total_width = vim.o.columns
  local total_height = vim.o.lines

  local win_width = math.floor(total_width * ov_opts.width)
  local win_height = math.floor(total_height * ov_opts.height)

  local row = math.floor((total_height - win_height) / 2)
  local col = math.floor((total_width - win_width) / 2)
  local opts = {
    relative = "editor",
    width = win_width,
    height = win_height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
  }

  if O.opts.float_buf and vim.api.nvim_buf_is_valid(O.opts.float_buf) then
    if O.opts.float_win and vim.api.nvim_win_is_valid(O.opts.float_win) then
      vim.api.nvim_set_current_win(O.opts.float_win)
      vim.cmd.startinsert()
      return O.opts.float_buf
    else
      O.opts.float_win = vim.api.nvim_open_win(O.opts.float_buf, true, opts)
      vim.cmd.startinsert()
      return O.opts.float_buf
    end
  end

  O.opts.float_buf = vim.api.nvim_create_buf(false, true)

  O.opts.float_win = vim.api.nvim_open_win(O.opts.float_buf, true, opts)

  vim.bo[O.opts.float_buf].filetype = "terminal"

  O.opts.terminal_id = vim.fn.jobstart(command, {
    term = true,
    on_exit = function(_, exit_code)
      if exit_code == 0 then
        vim.api.nvim_win_close(O.opts.float_win, true)
        vim.api.nvim_buf_delete(O.opts.float_buf, { force = true })
      end
    end,
  })
  vim.cmd.startinsert()

  vim.keymap.set("t", "<M-q>", function()
    if O.opts.float_win and vim.api.nvim_win_is_valid(O.opts.float_win) then
      -- vim.api.nvim_win_close(M.opts.float_win, true)
      vim.api.nvim_win_hide(O.opts.float_win)
    end
  end, { buffer = O.opts.float_buf, nowait = true, silent = true })

  return O.opts.float_buf
end

function M.open_float_window_with_keymap(O, content)
  local calculate_columns = function(columns)
    local width = math.floor(columns * O.opts.width)
    return vim.o.columns - width
  end

  local opts = {
    relative = "editor",
    width = math.floor(vim.o.columns * O.opts.width),
    height = math.floor(vim.o.lines * O.opts.height),
    row = 0,
    col = calculate_columns(vim.o.columns),
    style = "minimal",
    border = "rounded",
  }

  if O.opts.float_buf and vim.api.nvim_buf_is_valid(O.opts.float_buf) then
    if O.opts.float_win and vim.api.nvim_win_is_valid(O.opts.float_win) then
      vim.api.nvim_set_current_win(O.opts.float_win)
      return O.opts.float_buf
    else
      O.opts.float_win = vim.api.nvim_open_win(O.opts.float_buf, true, opts)
      return O.opts.float_buf
    end
  end

  O.opts.float_buf = vim.api.nvim_create_buf(false, true)
  O.opts.float_win = vim.api.nvim_open_win(O.opts.float_buf, true, opts)

  vim.bo[O.opts.float_buf].modifiable = true
  vim.api.nvim_buf_set_lines(O.opts.float_buf, 0, -1, false, content or { "No content" })
  vim.bo[O.opts.float_buf].modifiable = false

  vim.bo[O.opts.float_buf].readonly = true
  vim.wo[O.opts.float_win].wrap = true;
  vim.bo[O.opts.float_buf].buftype = "nofile"
  vim.bo[O.opts.float_buf].bufhidden = "wipe"
  vim.bo[O.opts.float_buf].filetype = "jj"

  vim.keymap.set("n", "q", function()
    if O.opts.float_win and vim.api.nvim_win_is_valid(O.opts.float_win) then
      vim.api.nvim_win_close(O.opts.float_win, true)
    end
  end, { buffer = O.opts.float_buf, nowait = true, silent = true })

  return O.opts.float_buf
end

return M
