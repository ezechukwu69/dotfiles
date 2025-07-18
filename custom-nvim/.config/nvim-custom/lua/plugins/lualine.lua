local function dir_name()
  local dirname = vim.fn.system("echo $(basename " .. vim.fn.getcwd() .. ")")
  return "  " .. vim.trim(dirname)
end

return {
  'nvim-lualine/lualine.nvim',
  event = "BufRead",
  dependencies = { { 'nvim-tree/nvim-web-devicons' } },
  config = function()
    local opts = {
      theme = "auto",
      options = {
        section_separators = "",
        component_separators = "",
        globalstatus = true,
        always_divide_middle = false,
      },
      disabled_filetypes = {
        -- statusline = { "Avante", "AvanteInput", "AvanteSelectedFiles" },
        winbar = {},
      },
      sections = {
        lualine_c = { function() return "󰈔 " end, 'filename' },
        lualine_x = {
          dir_name,
          'encoding',
          'fileformat',
          {
            "lsp_status",
            icon = '', -- f013
            symbols = {
              -- Standard unicode symbols to cycle through for LSP progress:
              spinner = { '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' },
              -- Standard unicode symbol for when LSP is done:
              done = '✓',
              -- Delimiter inserted between LSP names:
              separator = ' ',
            },
            -- List of LSP names to ignore (e.g., `null-ls`):
            ignore_lsp = {},
          },
          -- { "datetime", style = "%H:%M" }
        },
        lualine_y = { 'filetype' },
      },
      extensions = {
        "aerial",
        "assistant",
        "avante",
        "chadtree",
        "ctrlspace",
        "fern",
        "fugitive",
        "fzf",
        "lazy",
        "man",
        "mason",
        "mundo",
        "neo-tree",
        "nerdtree",
        "nvim-dap-ui",
        "nvim-tree",
        "oil",
        "overseer",
        "quickfix",
        "symbols-outline",
        "toggleterm",
        "trouble",
      }
    }
    return require("lualine").setup(opts)
  end
}

-- DEFAULTS
-- sections = {
--    lualine_a = {'mode'},
--    lualine_b = {'branch', 'diff', 'diagnostics'},
--    lualine_c = {'filename'},
--    lualine_x = {'encoding', 'fileformat', 'filetype'},
--    lualine_y = {'progress'},
--    lualine_z = {'location'}
--  },
--  inactive_sections = {
--    lualine_a = {},
--    lualine_b = {},
--    lualine_c = {'filename'},
--    lualine_x = {'location'},
--    lualine_y = {},
--    lualine_z = {}
--  },

--
-- branch (git branch)
-- buffers (shows currently available buffers)
-- diagnostics (diagnostics count from your preferred source)
-- diff (git diff status)
-- encoding (file encoding)
-- fileformat (file format)
-- filename
-- filesize
-- filetype
-- hostname
-- location (location in file in line:column format)
-- mode (vim mode)
-- progress (%progress in file)
-- searchcount (number of search matches when hlsearch is active)
-- selectioncount (number of selected characters or lines)
-- tabs (shows currently available tabs)
-- windows (shows currently available windows)
-- lsp_status (shows active LSPs in the current buffer and a progress spinner)
