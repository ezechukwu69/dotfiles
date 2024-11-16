local M = {}
M.base46 = {
  theme = "rosepine",
  transparency = false,
}
M.ui = {
  statusline = {
    enabled = true,
    theme = "minimal",
    separator_style = "round",
  },
  nvdash = {
    load_on_startup = true,
  },
  cmp = {
    icons_left = true,
    format_colors = {
      tailwind = true,
    },
  },
}
return M
