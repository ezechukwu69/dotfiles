return {
  {
    "shortcuts/no-neck-pain.nvim",
    version = "*",
    event = "VeryLazy",
    config = function()
      require("no-neck-pain").setup {
        buffers = {
          right = {
            enabled = false,
          },
          scratchPad = {
            -- set to `false` to
            -- disable auto-saving
            enabled = true,
            -- set to `nil` to default
            -- to current working directory
            location = "~/Documents/scratchpad/",
          },
          bo = {
            filetype = "markdown",
          },
          wo = {
            fillchars = "eob: ",
          },
        },
      }
      require("which-key").add {
        { "<leader>n", group = "NoNeckPain" },
        { "<leader>nn", "<cmd>NoNeckPain<cr>", desc = "No Neck Pain" },
        { "<leader>ns", "<cmd>NoNeckPainScratchPad<cr>", desc = "No Neck Pain ScratchPad" },
      }
    end,
  },
}
