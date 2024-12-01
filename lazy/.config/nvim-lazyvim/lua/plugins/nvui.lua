return {
  {
    "nvchad/ui",
    config = function()
      require("nvchad")
      vim.keymap.set("n", "<leader>mt", "<cmd>lua require('nvchad.themes').open()<cr>", { desc = "Open Themes" })
      vim.keymap.set("n", "<leader>mo", function()
        require("menu").open("default")
      end, { desc = "Open Menu" })
      vim.keymap.set("n", "<leader>ms", "<cmd>Shades<cr>", { desc = "Toggle Shades" })
      vim.keymap.set("n", "<leader>mh", "<cmd>Huefy<cr>", { desc = "Toggle Huefy" })
    end,
  },

  {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  { "nvchad/volt", lazy = true },
  {
    "nvchad/menu",
    lazy = true,
  },

  {
    "nvchad/minty",
    cmd = { "Shades", "Huefy" },
    config = function()
      require("minty").setup({})
    end,
  },
}
