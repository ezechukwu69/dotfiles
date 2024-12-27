return {
  "joshuavial/aider.nvim",
  config = function()
    -- set a keybinding for the AiderOpen function
    vim.api.nvim_set_keymap("n", "<leader>ia", "<cmd>lua AiderOpen()<cr>", { noremap = true, silent = true })
    -- set a keybinding for the AiderBackground function
    vim.api.nvim_set_keymap("n", "<leader>ib", "<cmd>lua AiderBackground()<cr>", { noremap = true, silent = true })
    require("aider").setup({
      auto_manage_context = false,
      default_bindings = false,
    })
  end,
}
