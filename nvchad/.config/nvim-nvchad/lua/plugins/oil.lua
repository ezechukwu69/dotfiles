return {
  {
    "stevearc/oil.nvim",
    opts = {},
    enabled = true,
    event = "VeryLazy",
    -- Optional dependencies
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("oil").setup {
        default_file_explorer = false,
      }
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { noremap = true, desc = "Open parent directory" })
    end,
  },
}
