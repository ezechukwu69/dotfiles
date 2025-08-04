(import-macros {: map!} :macros)

(vim.pack.add ["https://github.com/folke/which-key.nvim"])


(local wk (require "which-key"))

(wk.setup {
    :preset "helix"
})

(map!
  :mode "n"
  :key "<leader>?"
  :command (fn []
             (wk.show {:global true}))
  :options {:desc "Buffer local keymaps (which-key)"}
)

