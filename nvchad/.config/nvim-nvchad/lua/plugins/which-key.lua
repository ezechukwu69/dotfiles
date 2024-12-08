return {
  "folke/which-key.nvim",
  opts = function(_, opts)
    return vim.tbl_deep_extend("force", opts, {
      preset = "helix",
    })
  end,
  lazy = false,
  init = function()
    local whickKey = require "which-key"
    whickKey.add {
      { "<leader>a", group = "AI" },
      { "<leader>f", group = "Find" },
      { "<leader>g", group = "Git" },
      { "<leader>m", group = "Marks" },
      { "<leader>p", group = "Picker / Projects" },
      { "<leader>r", group = "Rename" },
      { "<leader>s", group = "Signature" },
      { "<leader>t", group = "+ Test / Toggle" },
      { "<leader>w", group = "Workspace / Which Key" },
      { "<leader>z", group = "Zen Mode" },
    }
  end,
}
