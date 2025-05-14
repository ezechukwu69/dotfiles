return {
  "folke/snacks.nvim",
  --enabled = false,
  keys = {
    {
      "+",
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
    gitbrowse = {},
    dim = {},
    image = {},
    scope = {},
    statuscolumn = {
      left = { "mark", "sign" }, -- priority of signs on the left (high to low)
      right = { "fold", "git" }, -- priority of signs on the right (high to low)
      folds = {
        open = true, -- show open fold icons
        git_hl = true, -- use Git Signs hl for fold icons
      },
      git = {
        -- patterns to match Git signs
        patterns = { "GitSign", "MiniDiffSign" },
      },
      refresh = 50, -- refresh at most every 50ms
    },
    picker = {
      layout = {
        preset = "telescope", -- "ivy" | "vscode" | "select" | "sidebar" | "top" | "left" | "right" | "default" | "telescope" | "dropdown"
      },
      matcher = {
        fuzzy = true,
        frecency = true, -- frecency bonus
      },
      -- ui_select = true,
      debug = {
        scores = true,
      },
      layouts = {
        telescope = {
          reverse = false,
          layout = {
            box = "horizontal",
            backdrop = false,
            width = 0.8,
            height = 0.9,
            border = "none",
            {
              box = "vertical",
              { win = "list", title = " Results ", title_pos = "center", border = "rounded" },
              { win = "input", height = 1, border = "rounded", title = "{title} {live} {flags}", title_pos = "center" },
            },
            {
              win = "preview",
              title = "{preview:Preview}",
              width = 0.45,
              border = "rounded",
              title_pos = "center",
            },
          },
        },
        default = {
          layout = {
            backdrop = false,
            row = 1,
            width = 0.8,
            min_width = 80,
            height = 0.8,
            border = "rounded",
            box = "horizontal",
            {
              box = "vertical",
              border = "rounded",
              title = "{title} {live} {flags}",
              title_pos = "center",
              { win = "list", border = "bottom" },
              { win = "input", height = 1, border = "top" },
            },
            { win = "preview", title = "{preview}", width = 0.4, border = "rounded" },
          },
        },
        ivy = {
          layout = {
            box = "vertical",
            backdrop = false,
            row = -1,
            width = 0,
            height = 0.7,
            border = "top",
            title = " {title} {live} {flags}",
            title_pos = "left",
            { win = "input", height = 1, border = "bottom" },
            {
              box = "horizontal",
              { win = "list", border = "none" },
              { win = "preview", title = "{preview}", width = 0.5, border = "none" },
            },
          },
        },
        vscode = {
          preview = "main",
          layout = {
            backdrop = false,
            row = 1,
            width = 0.4,
            min_width = 80,
            height = 0.4,
            border = "none",
            box = "vertical",
            { win = "input", height = 1, border = "rounded", title = "{title} {live} {flags}", title_pos = "center" },
            { win = "list", border = "hpad" },
            { win = "preview", title = "{preview}", height = 0.9, border = "rounded" },
          },
        },
      },
      sources = {
        explorer = {
          finder = "explorer",
          sort = { fields = { "sort" } },
          tree = true,
          supports_live = true,
          follow_file = true,
          auto_close = true,
          jump = { close = false },
          -- layout = { preset = "ivy", preview = false },
        },
      },
    },
  },
}
