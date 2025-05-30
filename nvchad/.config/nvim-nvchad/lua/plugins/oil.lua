return {
  {
    "stevearc/oil.nvim",
    opts = {},
    lazy = true,
    event = "BufReadPre",
    -- Optional dependencies
    enabled = true,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("oil").setup({
        default_file_explorer = false,
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
    end,
  },
}
