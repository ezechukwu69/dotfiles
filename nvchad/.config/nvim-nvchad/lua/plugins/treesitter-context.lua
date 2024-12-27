return {
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "VeryLazy",
    opts = function()
      -- local tsc = require "treesitter-context"
      return { enable = true, mode = "cursor", max_lines = 3 }
    end,
  },
}
