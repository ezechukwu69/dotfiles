return {
  "christoomey/vim-tmux-navigator",
  cmd = {
    "TmuxNavigateLeft",
    "TmuxNavigateDown",
    "TmuxNavigateUp",
    "TmuxNavigateRight",
    "TmuxNavigatePrevious",
  },
  keys = {
    { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>", mode = { "n", "x", "o" } },
    { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>", mode = { "n", "x", "o" } },
    { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>", mode = { "n", "x", "o" } },
    { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>", mode = { "n", "x", "o" } },
    { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>", mode = { "n", "x", "o" } },
  },
}
