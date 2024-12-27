---@type ChadrcConfig
local M = {}

M.base46 = {
  theme = "ayu_dark",

  -- hl_override = {
  --   Comment = { italic = true },
  --   transparency = true,
  --   ["@comment"] = { italic = true },
  -- },
}

-- AI! add a autocommand that set relative line number when a file is opened

M.ui = {

  statusline = {
    enabled = false,
    theme = "default", -- default/vscode/vscode_colored/minimal
    -- default/round/block/arrow separators work only for default statusline theme
    -- round and block will work for minimal theme only
    separator_style = "round",
    order = nil,
    modules = nil,
  },

  tabufline = {
    enabled = true,
    lazyload = true,
    order = { "treeOffset", "buffers", "tabs", "btns" },
    modules = nil,
  },
}

M.lsp = {
  signature = false,
}

return M
