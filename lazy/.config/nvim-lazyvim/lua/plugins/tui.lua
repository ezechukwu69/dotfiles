return {
  "ezechukwu69/tui.nvim",
  config = function()
    require("tui").setup({
      name = "LazyGitTUI",
      command = "lazygit",
      height_margin = 2,
      width_margin = 2,
    })

    require("tui").setup({
      name = "LazyYazi",
      command = "yazi",
      height_margin = 2,
      width_margin = 2,
    })

    require("tui").setup({
      name = "Btop",
      command = "btop",
      height_margin = 2,
      width_margin = 2,
    })

    require("tui").setup({
      name = "LazyDocker",
      command = "lazydocker",
      height_margin = 2,
      width_margin = 2,
    })

    require("tui").setup({
      name = "LazyNpm",
      command = "lazynpm",
      height_margin = 2,
      width_margin = 2,
    })
  end,
  keys = {
    { "<leader>tg", "<cmd>LazyGitTUI<cr>", desc = "[T]oggle LazyGit TUI" },
    { "<leader>ty", "<cmd>LazyYazi<cr>", desc = "[T]oggle Yazi TUI" },
    { "<leader>tb", "<cmd>Btop<cr>", desc = "[T]oggle Btop TUI" },
    { "<leader>td", "<cmd>LazyDocker<cr>", desc = "[T]oggle LazyDocker TUI" },
    { "<leader>tn", "<cmd>LazyNpm<cr>", desc = "[T]oggle LazyNpm TUI" },
  },
}
