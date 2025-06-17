return {
  'echasnovski/mini.nvim',
  dependencies = {
    'nvim-treesitter/nvim-treesitter',
    'nvim-treesitter/nvim-treesitter-textobjects',
    'nvim-treesitter/playground',
  },
  version = false,
  event = "VeryLazy",
  config = function()
    -- local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
    -- parser_config.org = {
    --   install_info = {
    --     url = 'https://github.com/milisims/tree-sitter-org',
    --     revision = 'main',
    --     files = { 'src/parser.c', 'src/scanner.c' },
    --   },
    --   filetype = 'org',
    -- }
    require('nvim-treesitter.configs').setup {
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<M-=>", -- set to `false` to disable one of the mappings
          node_incremental = "<M-=>",
          scope_incremental = "<M-s>",
          node_decremental = "<M-->",
        }
      },
      indent = {
        enable = true,
      }
    }
    require("mini.ai").setup {
      -- Table with textobject id as fields, textobject specification as values.
      -- Also use this to disable builtin textobjects. See |MiniAi.config|.
      custom_textobjects = nil,

      -- Module mappings. Use `''` (empty string) to disable one.
      mappings = {
        -- Main textobject prefixes
        around = 'a',
        inside = 'i',

        -- Next/last variants
        around_next = 'an',
        inside_next = 'in',
        around_last = 'al',
        inside_last = 'il',

        -- Move cursor to corresponding edge of `a` textobject
        goto_left = 'g[',
        goto_right = 'g]',
      },
      -- Number of lines within which textobject is searched
      n_lines = 50,

      -- How to search for object (first inside current line, then inside
      -- neighborhood). One of 'cover', 'cover_or_next', 'cover_or_prev',
      -- 'cover_or_nearest', 'next', 'previous', 'nearest'.
      search_method = 'cover_or_next',

      -- Whether to disable showing non-error feedback
      -- This also affects (purely informational) helper messages shown after
      -- idle time if user input is required.
      silent = false,
    }
    require('mini.bufremove').setup()
  end
}
