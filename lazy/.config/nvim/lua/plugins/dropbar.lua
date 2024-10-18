return {
  {
    "Bekaboo/dropbar.nvim",
    lazy = false,
    -- optional, but required for fuzzy finder support
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
    },
    config = function()
      vim.keymap.set("n", "<leader>mb", "<cmd>lua require('dropbar.api').pick()<cr>", { desc = "Toggle Dropbar" })
    end,
  },
}
