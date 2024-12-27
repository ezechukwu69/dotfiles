return {
  {
    "jackMort/tide.nvim",
    event = "VeryLazy",
    opts = {
      -- optional configuration
      keys = {
        leader = ";",
      },
    },
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
  },
}
