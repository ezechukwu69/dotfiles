return {
  "echasnovski/mini.nvim",
  event = "VeryLazy",
  config = function()
    local which_key = require "which-key"
    which_key.add {
      -- { "-",          "<cmd>lua MiniFiles.open()<cr>",             desc = "Open Files" },
      { "<leader>gs", "<cmd>lua MiniGit.show_at_cursor()<cr>",     desc = "Show Git at Cursor" },
      { "<leader>gd", "<cmd>lua MiniGit.show_diff_source()<cr>",   desc = "Show Git Diff" },
      { "<leader>gh", "<cmd>lua MiniGit.show_range_history()<cr>", desc = "Show Git History" },
    }
    require("mini.ai").setup()
    require("mini.diff").setup()
    -- require("mini.files").setup {
    --   options = {
    --     -- Whether to delete permanently or move into module-specific trash
    --     permanent_delete = false,
    --     -- Whether to use for editing directories
    --     use_as_default_explorer = true,
    --   },
    --   windows = {
    --     -- Maximum number of windows to show side by side
    --     max_number = math.huge,
    --     -- Whether to show preview of file/directory under cursor
    --     preview = true,
    --     -- Width of focused window
    --     width_focus = 50,
    --     -- Width of non-focused window
    --     width_nofocus = 15,
    --     -- Width of preview window
    --     width_preview = 50,
    --   },
    --   mappings = {
    --     close = "q",
    --     go_in = "l",
    --     go_in_plus = "<cr>",
    --     go_out = "h",
    --     go_out_plus = "H",
    --     mark_goto = "'",
    --     mark_set = "m",
    --     reset = "<BS>",
    --     reveal_cwd = "@",
    --     show_help = "g?",
    --     synchronize = "=",
    --     trim_left = "<",
    --     trim_right = ">",
    --   },
    -- }
    require("mini.git").setup()
    require("mini.indentscope").setup()
    require("mini.move").setup()
    require("mini.surround").setup({
      mappings = {
        add = "ms",
        delete = "md",    -- Delete surrounding
        find = "mf",      -- Find surrounding (to the right)
        find_left = "mF", -- Find surrounding (to the left)
        highlight = nil,  -- Highlight surrounding
        replace = "mr",   -- Replace surrounding
        update_n_lines = "mu",
      }
    })
  end,
}
