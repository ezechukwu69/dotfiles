return {
  "ezechukwu69/detacher.nvim",
  -- dir = "~/development/lua/aidrun.nvim",
  config = function()
    local keymap = vim.keymap.set
    keymap("n", "<space>qe", ":Detach<CR>", { desc = "Detach from neovim" })

    require("detacher").setup()
  end,
}
