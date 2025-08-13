(require :plugins.colorschemes)
(vim.pack.add [
   "https://github.com/stevearc/dressing.nvim" 
   "https://github.com/nvim-lua/plenary.nvim" 
   "https://github.com/nvim-neotest/nvim-nio" 
   "https://github.com/MunifTanjim/nui.nvim" 
   {
      :src "https://github.com/L3MON4D3/LuaSnip"
      :version (vim.version.range "v2.*")
    }
])
(require "plugins/treesitter")
(require "plugins/mini")
(require "plugins/snacks")
(require "plugins/which-key")
(require "plugins/lualine")
(require "plugins/markdown")
(require "plugins/dap")
(require "plugins/hunk")
(require "plugins/supermaven")
(require "plugins/edgy-custom")
(require "plugins/flutter-tools")
(require "plugins/carenvim")
