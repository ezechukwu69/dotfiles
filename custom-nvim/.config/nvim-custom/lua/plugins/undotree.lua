return {
  "mbbill/undotree",
  event = "BufReadPost",
  dependencies = {
    {
      "MunifTanjim/nui.nvim",
    }
  },
  keys = {
    { "<leader>uu", "<cmd>UndotreeToggle<cr>", mode = "n", desc = "Toggle undo tree" },
  }
}
