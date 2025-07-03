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

vim.g.autoformat_enabled = true

autocmd({ "BufWritePre" }, {
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

autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("DiagLspAttach", {}),
  callback = function(ev)
    local map = vim.keymap.set
    local buf = ev.buf
    map("n", "gd", function()
      vim.lsp.buf.definition()
    end, { desc = "Go to definition", buffer = buf })
    -- local client = vim.lsp.get_client_by_id(ev.data.client_id)
    -- if client:supports_method('textDocument/completion') then
    --     vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    -- end
  end
})

vim.api.nvim_create_autocmd('User', {
  pattern = 'MiniFilesBufferCreate',
  callback = function(args)
    local filter_show = function(fs_entry) return true end

    local filter_hide = function(fs_entry)
      return not vim.startswith(fs_entry.name, '.')
    end
    local buf_id = args.data.buf_id
    vim.g.show_mini_dotfiles = true
    local toggle_dotfiles = function()
      vim.g.show_mini_dotfiles = not vim.g.show_mini_dotfiles
      local new_filter = vim.g.show_mini_dotfiles and filter_show or filter_hide
      MiniFiles.refresh({ content = { filter = new_filter } })
    end
    -- Tweak left-hand side of mapping to your liking
    vim.keymap.set('n', 'g.', toggle_dotfiles, { buffer = buf_id })
  end,
})

local userconfig = vim.api.nvim_create_augroup("UserConfig", {})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = userconfig,
  callback = function()
    vim.hl.on_yank { higroup = "IncSearch", timeout = 400 }
  end,
})
