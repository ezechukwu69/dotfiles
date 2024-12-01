return {
  {
    "supermaven-inc/supermaven-nvim",
    -- opts = {
    --   windows = { ghost_text = { enabled = true } },
    -- },
    event = "VeryLazy",
    enabled = true,
    opts = {
      windows = { ghost_text = { enabled = true } },
      keymaps = {
        accept_suggestion = "<C-q>",
        clear_suggestion = "<C-x>",
        accept_word = "<C-y>",
      }, -- handled by nvim-cmp / blink.cmp
    },
  },
}
