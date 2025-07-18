local function starts_with(str, prefix)
  return string.sub(str, 1, #prefix) == prefix
end
return {
  'echasnovski/mini.nvim',
  -- enabled = false,
  keys = {
    {
      '-',
      function()
        local file = vim.api.nvim_buf_get_name(0)
        if file and file ~= '' and not starts_with(file, "minifiles:/") then
          vim.notify(file)
          MiniFiles.open(file, false)
        else
          MiniFiles.open()
        end
      end,
      desc = 'MiniFiles',
      mode = 'n'
    },
  },
  config = function()
    require("mini.pairs").setup()
    local spec_treesitter = require("mini.ai").gen_spec.treesitter
    require("mini.ai").setup {
      -- Table with textobject id as fields, textobject specification as values.
      -- Also use this to disable builtin textobjects. See |MiniAi.config|.
      custom_textobjects = {
        f = spec_treesitter({ a = {
          "@function.outer",
          "@method.outer"
        }, i = {
          "@function.inner",
          "@method.inner"
        } }),
        c = spec_treesitter({ a = "@class.outer", i = "@class.inner" }),
        C = spec_treesitter({ a = "@comment.outer", i = "@comment.inner" }),
        a = spec_treesitter({ a = "@parameter.inner", i = "@parameter.inner" }),
        o = spec_treesitter({ a = {
          "@conditional.outer",
          "@loop.outer"
        }, i = {
          "@conditional.inner",
          "@loop.inner"
        } }),
        p = spec_treesitter({ a = {
          "@field.outer",
          "@property.outer",
        }, i = {
          "@field.inner",
          "@property.inner",
        } }),
        v = spec_treesitter({ a = "@variable.outer", i = "@variable.inner" }),
      },

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
    require('mini.files').setup({
      mappings = {
      },
      options = {
        use_as_default_explorer = true,
        permanently_delete = false,
      }
    })
    require('mini.surround').setup {
      mappings = {
        add = 'sa',            -- Add surrounding in Normal and Visual modes
        delete = 'sd',         -- Delete surrounding
        find = 'sf',           -- Find surrounding (to the right)
        find_left = 'sF',      -- Find surrounding (to the left)
        highlight = 'sh',      -- Highlight surrounding
        replace = 'sr',        -- Replace surrounding
        update_n_lines = 'sn', -- Update `n_lines`

        suffix_last = 'l',     -- Suffix to search with "prev" method
        suffix_next = 'n',     -- Suffix to search with "next" method
      }
    }
  end,
  init = function()
    require("mini.snippets").setup({

    })
    -- require("mini.sessions").setup {
    --   autoread = true,
    --   autowrite = true,
    --   direcrory = vim.fn.stdpath("data") .. "/sessions",
    --   file = "Session.vim",
    --   verbose = { read = true, write = true, delete = true },
    -- }
    local logo = vim.fn.system("toilet -f smmono9 -F border ' Ezechukwu69 '")

    local starter = require('mini.starter')
    local delta = (vim.loop.hrtime() - _G.nvim_start_time) / 1e6
    starter.setup {
      autoopen = true,
      header = logo,
      items = {
        starter.sections.builtin_actions(),
        -- starter.sections.recent_files(10, false),
        starter.sections.recent_files(5, true),
        -- starter.sections.pick(),
        -- Use this if you set up 'mini.sessions'
        -- starter.sections.sessions(5, true)
      },
      content_hooks = {
        starter.gen_hook.adding_bullet(),
        starter.gen_hook.aligning("center", "center"),
        starter.gen_hook.padding(3, 2),
      },
      footer = string.format("Loaded in %.2f ms", delta),
    }
  end
}
