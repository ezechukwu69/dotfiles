return {
  {
    "supermaven-inc/supermaven-nvim",
    -- opts = {
    --   windows = { ghost_text = { enabled = true } },
    -- },
    opts = {
      windows = { ghost_text = { enabled = true } },
      disable_inline_completion = not vim.g.ai_cmp,
      keymaps = {
        accept_suggestion = "<Tab>",
        clear_suggestion = "<C-[>",
        accept_word = "<C-]>",
      }, -- handled by nvim-cmp / blink.cmp
    },
  },
}
