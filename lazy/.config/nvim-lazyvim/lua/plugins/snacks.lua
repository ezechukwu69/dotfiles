return {
  "folke/snacks.nvim",
  --enabled = false,
  keys = {
    {
      "-",
      function()
        Snacks.picker.explorer()
      end,
      desc = "Open Snacks explorer (Directory of Current File)",
    },
    {
      "gs",
      function()
        Snacks.picker.lsp_symbols()
      end,
      desc = "Open Snacks lsp symbols",
    },
    {
      "gS",
      function()
        Snacks.picker.lsp_workspace_symbols()
      end,
      desc = "Open Snacks lsp workspace symbols",
    },
    {
      "<leader>sp",
      function()
        Snacks.picker()
      end,
      desc = "Open Snack picker",
    },
  },
  ---@type snacks.Config
  opts = {
    words = {},
    git = {},
    dim = {},
    scope = {},
    picker = {
      sources = {
        explorer = {
          finder = "explorer",
          sort = { fields = { "sort" } },
          tree = true,
          supports_live = true,
          follow_file = true,
          auto_close = true,
          jump = { close = false },
          layout = { preset = "sidebar", preview = false },
        },
      },
    },
  },
}
