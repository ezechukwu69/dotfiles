return {
  "folke/which-key.nvim",
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
  config = function()
    local opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      preset = "helix",
    }
    require("which-key").setup(opts)
  end,
}
