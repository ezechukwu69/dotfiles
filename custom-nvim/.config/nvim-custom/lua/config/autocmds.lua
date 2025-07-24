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
    local ft_to_lsp = {
      javascript = "vtsls",
      typescript = "vtsls",
      lua = "luals",
      html = "htmlls",
      eruby = "htmlls",
      python = "pyright",
      go = "gopls",
    }

    local ft = vim.bo[ev.buf].filetype
    local preferred_client = ft_to_lsp[ft]

    if not vim.g.autoformat_enabled then
      return
    end

    local buf = ev.buf
    -- local clients = vim.lsp.get_clients({ bufnr = buf })

    vim.lsp.buf.format({
      bufnr = buf,
      filter = function(client)
        if preferred_client then
          vim.notify("Formatting with " .. preferred_client)
          return client.name == preferred_client
        end

        local is_formattable = client:supports_method "textDocument/formatting"

        if is_formattable then
          vim.notify("Formatting! with " .. client.name)
        end
        return is_formattable
      end
    })
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

    map("n", "<leader>wa", function()
      vim.lsp.buf.add_workspace_folder()
    end, { desc = "Add Workspace Folder" })
    map("n", "<leader>wl", function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, { desc = "Add Workspace Folder" })
    map("n", "<leader>wr", function()
      vim.lsp.buf.remove_workspace_folder()
    end, { desc = "Remove Workspace Folder" })
    -- local client = vim.lsp.get_client_by_id(ev.data.client_id)
    -- if client:supports_method('textDocument/completion') then
    --     vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    -- end
  end
})

autocmd("FileType", {
  group = vim.api.nvim_create_augroup("QuitWithQ", {}),
  pattern = {
    "qf",
    "help",
    "neotest-output",
  },
  callback = function(args)
    local map = vim.keymap.set
    map("n", "q", function()
      vim.cmd("q")
    end, { buffer = args.buf })
  end
})


autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspCompletion", {}),
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if not client then
      return
    end
    if client:supports_method('textDocument/completion') then
      vim.opt.completeopt = "menuone,menu,noinsert,fuzzy,popup,preview"
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
      vim.keymap.set("i", "<C-Space>", function()
        vim.lsp.completion.get()
      end)
    end
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
