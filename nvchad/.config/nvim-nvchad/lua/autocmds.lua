require "nvchad.autocmds"


local function split(inputstr, sep)
  sep = sep or "%s" -- default to whitespace
  local t = {}
  for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
    table.insert(t, str)
  end
  return t
end

local autocmd = vim.api.nvim_create_autocmd

autocmd("BufReadPost", {
  pattern = "*",
  callback = function()
    local line = vim.fn.line "'\""
    if
        line > 1
        and line <= vim.fn.line "$"
        and vim.bo.filetype ~= "commit"
        and vim.fn.index({ "xxd", "gitrebase" }, vim.bo.filetype) == -1
    then
      vim.cmd 'normal! g`"'
    end
  end,
})

autocmd("BufDelete", {
  callback = function()
    local bufs = vim.t.bufs
    if #bufs == 1 and vim.api.nvim_buf_get_name(bufs[1]) == "" then
      vim.cmd "Nvdash"
    end
  end,
})

vim.g.autoformat_enabled = true

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("NvchadWritePre", {}),
  callback = function(ev)
    if not vim.g.autoformat_enabled then
      return
    end
    local buf = ev.buf
    local clients = vim.lsp.get_clients({ bufnr = buf })
    for _, client in ipairs(clients) do
      if client:supports_method("textDocument/formatting", buf) then
        vim.lsp.buf.format({ bufnr = buf })
      end
    end
  end,
})

autocmd("FileType", {
  pattern = "qf",
  callback = function()
    local map = vim.keymap.set
    map("n", "<localleader>f", function()
      local filter = vim.fn.input({ prompt = "Filter: " })
      if not filter or filter == "" then
        return
      end
      vim.cmd("Cfilter " .. filter)
    end, { desc = "Filter quickfix", buffer = true })

    map("n", "<localleader>F", function()
      local filter = vim.fn.input({ prompt = "Exclude: " })
      if not filter or filter == "" then
        return
      end
      vim.cmd("Cfilter! " .. filter)
    end, { desc = "Filter quickfix (exclude)", buffer = true })

    map("n", "<localleader>e", function()
      -- get current line highlighted
      local line = vim.fn.getline "."
      local part = split(line, "|")[3]
      vim.cmd("Cfilter! " .. part)
    end, { desc = "Filter quickfix (exclude)", buffer = true })
  end
})
