return {
  {
    "supermaven-inc/supermaven-nvim",
    -- opts = {
    --   windows = { ghost_text = { enabled = true } },
    -- },
    -- enabled = false,
    opts = {
      windows = { ghost_text = { enabled = true } },
      disable_inline_completion = not vim.g.ai_cmp,
      keymaps = {
        accept_suggestion = "<M-l>",
        clear_suggestion = "<C-[>",
        accept_word = "<C-]>",
      }, -- handled by nvim-cmp / blink.cmp
    },
  },
}
