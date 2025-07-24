return {
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
    },
    config = function()
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
        textobjects = {
          swap = {
            enable = true,
            swap_next = {
              ["<leader>ma"] = "@parameter.inner",
              ["<leader>mf"] = "@parameter.outer",
            },
            swap_previous = {
              ["<leader>mA"] = "@parameter.inner",
              ["<leader>mF"] = "@function.outer",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- Record jumps in jumplist
            goto_next_start = {
              ["]f"] = "@function.outer",
              ["]c"] = "@class.outer",
              ["]a"] = "@parameter.inner",
            },
            goto_next_end = {
              ["]F"] = "@function.outer",
              ["]C"] = "@class.outer",
              ["]A"] = "@parameter.inner",
            },
            goto_previous_start = {
              ["[f"] = "@function.outer",
              ["[c"] = "@class.outer",
              ["[a"] = "@parameter.inner",
            },
            goto_previous_end = {
              ["[F"] = "@function.outer",
              ["[C"] = "@class.outer",
              ["[A"] = "@parameter.inner",
            },
          },
          lsp_interop = {
            enable = true,
            border = 'rounded',
            peek_definition_code = {
              ["<leader>pf"] = "@function.outer",
              ["<leader>pc"] = "@class.outer",
              ["<leader>pi"] = "@identifier.outer",
            },
          },
        },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        indent = {
          enable = true,
        }
      }
    end
  },
  {
    'nvim-treesitter/nvim-treesitter-textobjects',
  },
  {
    'nvim-treesitter/playground',
  },
}
