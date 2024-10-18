return {
  "prichrd/netrw.nvim",
  opts = {},
  config = function()
    require("netrw").setup({})
    vim.keymap.set("n", "-", "<CMD>e .<cr>", { desc = "Open parent directory" })
  end,
}
