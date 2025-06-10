return {
  {
    "stevearc/oil.nvim",
    opts = {},
    lazy = true,
    event = "VeryLazy",
    -- Optional dependencies
    enabled = true,
    keys = {
      { "-", "<CMD>Oil<CR>", desc = "Open parent directory", mode = { "n" } },
    },
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("oil").setup({
        default_file_explorer = false,
      })
    end,
  },
}
