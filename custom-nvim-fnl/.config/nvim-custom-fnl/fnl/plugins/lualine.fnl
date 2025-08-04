(local dir_name (fn []
  (local dirname (vim.fn.system "echo $(basename "  (vim.fn.getcwd)  ")"))
  "  "  (vim.trim dirname)
  ))

(vim.pack.add [
               "https://github.com/nvim-lualine/lualine.nvim"
               "https://github.com/nvim-tree/nvim-web-devicons"
 ])

(local opts
  {:theme "auto"
   :options {:section_separators ""
             :component_separators ""
             :globalstatus true
             :always_divide_middle false}
   :disabled_filetypes {:winbar {}}
   :sections {:lualine_c [(fn [] "󰈔 ") "filename"]
              :lualine_x [dir_name
                          "encoding"
                          "fileformat"
                          ["lsp_status"
                          { :icon "" ; f013
                             :symbols {:spinner ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
                                       :done "✓"
                                       :separator " "}
                             :ignore_lsp {}}
                           ]
                          ; {:datetime true :style "%H:%M"}
                          ]
              :lualine_y ["filetype"]}
   :extensions ["aerial"
                "assistant"
                "avante"
                "chadtree"
                "ctrlspace"
                "fern"
                "fugitive"
                "fzf"
                "lazy"
                "man"
                "mason"
                "mundo"
                "neo-tree"
                "nerdtree"
                "nvim-dap-ui"
                "nvim-tree"
                "oil"
                "overseer"
                "quickfix"
                "symbols-outline"
                "toggleterm"
                "trouble"]})

(local lualine (require "lualine"))
(lualine.setup opts)
