---@type ChadrcConfig
local M = {}

M.base46 = {
  theme = "ashes",

  hl_override = {
    Comment = { italic = true },
    ["@comment"] = { italic = true },
  },
}

M.lsp = {
  signature = false,
}

return M
