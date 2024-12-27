return {
  {
    "mrjones2014/legendary.nvim",
    dependencies = { "kkharji/sqlite.lua" },
    priority = 10000,
    lazy = false,
    key = {
      { "<leader>sL", "<cmd>lua require('legendary').toggle()<cr>", { desc = "Legendary: Toggle Legendary" } },
    },
    config = function()
      require("which-key").add {
        {
          mode = "n",
          {
            "<leader>sL",
            function()
              require("legendary").find()
            end,
            desc = "Legendary: Open Legendary",
          },
        },
      }
      require("legendary").setup {
        extensions = {
          lazy_nvim = true,
          -- which_key = true,
        },
      }
    end,
  },
}
