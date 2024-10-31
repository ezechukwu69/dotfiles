return {
  -- Lua
  {
    "folke/zen-mode.nvim",
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    init = function()
      vim.keymap.set("n", "<leader>zz", ":ZenMode<CR>", { desc = "Zen Mode" })
    end,
  },
}
