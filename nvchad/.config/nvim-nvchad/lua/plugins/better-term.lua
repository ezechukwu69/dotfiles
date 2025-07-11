return {
  "CRAG666/betterTerm.nvim",
  enabled = false,
  opts = {
    position = "bot",
    size = 15,
  },
  keys = {
    {
      "<C-;>",
      function()
        require("betterTerm").open()
      end,
      desc = "Open terminal",
      mode = { "n", "t" },
    },
    {
      "<leader>tt",
      function()
        require("betterTerm").select()
      end,
      desc = "Select Terminal",
    },
  },
}
