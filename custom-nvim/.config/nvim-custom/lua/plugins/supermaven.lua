return {
  {
    "supermaven-inc/supermaven-nvim",
    keys = {
      { "<Esc>", "<Esc>", desc = "Escape" },
    },
    opts = {
      -- windows = { ghost_text = { enabled = true } },
      disable_inline_completion = false,
      keymaps = {
        accept_suggestion = "<M-l>",
        clear_suggestion = "<M-[>",
        accept_word = "<M-]>",
      }, -- handled by nvim-cmp / blink.cmp
    },
  },
}
