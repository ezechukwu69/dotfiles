return {
  {
    "nvchad/base46",
    lazy = false,
    build = function()
      require("base46").load_all_highlights()
    end,
  },
  "nvim-lua/plenary.nvim",
  { "nvim-tree/nvim-web-devicons", lazy = true },
  {
    "NvChad/NvChad",
    lazy = false,
    priority = 7000,
    branch = "v2.5",
  },
  {
    "nvchad/ui",
    lazy = false,
    config = function()
      require("nvchad")
    end,
  },

  "nvzone/volt",
  "nvzone/menu",
  { "nvzone/minty", cmd = { "Huefy", "Shades" } },
}
