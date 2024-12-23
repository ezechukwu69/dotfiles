return {
  {
    "sho-87/kanagawa-paper.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
    config = function()
      -- require("kanagawa-paper").setup({
      --   undercurl = true,
      --   transparent = false,
      --   gutter = false,
      --   dimInactive = true, -- disabled when transparent
      --   terminalColors = true,
      --   commentStyle = { italic = true },
      --   functionStyle = { italic = false },
      --   keywordStyle = { italic = false, bold = false },
      --   statementStyle = { italic = false, bold = false },
      --   typeStyle = { italic = false },
      --   colors = { theme = {}, palette = {} }, -- override default palette and theme colors
      --   overrides = function() -- override highlight groups
      --     return {}
      --   end,
      -- })
      -- vim.cmd("colorscheme kanagawa-paper")
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1000,
    -- opts = {},
    config = function()
      -- Default options:
      -- require("kanagawa").setup({
      --   compile = false, -- enable compiling the colorscheme
      --   undercurl = true, -- enable undercurls
      --   commentStyle = { italic = true },
      --   functionStyle = {},
      --   keywordStyle = { italic = true },
      --   statementStyle = { bold = true },
      --   typeStyle = {},
      --   transparent = false, -- do not set background color
      --   dimInactive = false, -- dim inactive window `:h hl-NormalNC`
      --   terminalColors = true, -- define vim.g.terminal_color_{0,17}
      --   colors = { -- add/modify theme and palette colors
      --     palette = {},
      --     theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
      --   },
      --   overrides = function(colors) -- add/modify highlights
      --     return {}
      --   end,
      --   theme = "wave", -- Load "wave" theme when 'background' option is not set
      --   background = { -- map the value of 'background' option to a theme
      --     dark = "wave", -- try "dragon" !
      --     light = "lotus",
      --   },
      -- })
      --
      -- -- setup must be called before loading
      -- vim.cmd("colorscheme kanagawa")
    end,
  },

  {
    "Shatur/neovim-ayu",
    lazy = false,
    priority = 1000,
    -- opts = {},
    config = function()
      -- Default options:
      require("ayu").setup({
        mirage = false, -- Set to `true` to use `mirage` variant instead of `dark` for dark background.
        terminal = true, -- Set to `false` to let terminal manage its own colors.
        overrides = {}, -- A dictionary of group names, each associated with a dictionary of parameters (`bg`, `fg`, `sp` and `style`) and colors in hex.
      })

      -- setup must be called before loading
      -- vim.cmd("colorscheme ayu-dark")
    end,
  },
  {
    "vague2k/vague.nvim",
    enabled = true,
    config = function()
      require("vague").setup({
        transparent = false, -- don't set background
        style = {
          -- "none" is the same thing as default. But "italic" and "bold" are also valid options
          boolean = "none",
          number = "none",
          float = "none",
          error = "none",
          comments = "italic",
          conditionals = "none",
          functions = "none",
          headings = "bold",
          operators = "none",
          strings = "italic",
          variables = "none",

          -- keywords
          keywords = "none",
          keyword_return = "none",
          keywords_loop = "none",
          keywords_label = "none",
          keywords_exception = "none",

          -- builtin
          builtin_constants = "none",
          builtin_functions = "none",
          builtin_types = "none",
          builtin_variables = "none",
        },
        -- Override colors
        colors = {
          bg = "#18191a",
          fg = "#cdcdcd",
          floatBorder = "#878787",
          line = "#282830",
          comment = "#646477",
          builtin = "#bad1ce",
          func = "#be8c8c",
          string = "#deb896",
          number = "#d2a374",
          property = "#c7c7d4",
          constant = "#b4b4ce",
          parameter = "#b9a3ba",
          visual = "#363738",
          error = "#d2788c",
          warning = "#e6be8c",
          hint = "#8ca0dc",
          operator = "#96a3b2",
          keyword = "#7894ab",
          type = "#a1b3b9",
          search = "#465362",
          plus = "#8faf77",
          delta = "#e6be8c",
        },
      })
    end,
  },

  -- {
  --   "LazyVim/LazyVim",
  --   opts = {
  --     colorscheme = "ayu-dark",
  --   },
  -- },
}
