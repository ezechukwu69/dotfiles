(vim.pack.add ["https://github.com/nvim-treesitter/nvim-treesitter" "https://github.com/nvim-treesitter/nvim-treesitter-textobjects" "https://github.com/nvim-treesitter/playground" ])

(local treesitter (require :nvim-treesitter.configs))

(treesitter.setup
  {
   :highlight
   {:enable true
    :additional_vim_regex_highlighting false}
   :incremental_selection
   {:enable true
    :keymaps
    {:init_selection "<M-=>"
     :node_incremental "<M-=>"
     :scope_incremental "<M-s>"
     :node_decremental "<M->>"}}
   :textobjects
   {:swap
    {:enable true
     :swap_next
     {["<leader>ma"] "@parameter.inner"
      ["<leader>mf"] "@parameter.outer"}
     :swap_previous
     {["<leader>mA"] "@parameter.inner"
      ["<leader>mF"] "@function.outer"}}
    :move
    {:enable true
     :set_jumps true
     :goto_next_start
     {["]f"] "@function.outer"
      ["]c"] "@class.outer"
      ["]a"] "@parameter.inner"}
     :goto_next_end
     {["]F"] "@function.outer"
      ["]C"] "@class.outer"
      ["]A"] "@parameter.inner"}
     :goto_previous_start
     {["[f"] "@function.outer"
      ["[c"] "@class.outer"
      ["[a"] "@parameter.inner"}
     :goto_previous_end
     {["[F"] "@function.outer"
      ["[C"] "@class.outer"
      ["[A"] "@parameter.inner"}}
    :lsp_interop
    {:enable true
     :border "rounded"
     :peek_definition_code
     {["<leader>pf"] "@function.outer"
      ["<leader>pc"] "@class.outer"
      ["<leader>pi"] "@identifier.outer"}}}
   :indent
   {:enable true}}
   )
