; (import-macros {: map!} :macros)

(vim.pack.add ["https://github.com/supermaven-inc/supermaven-nvim"])

((. (require :supermaven-nvim) :setup) {
    :disable_inline_completion false
    :keymaps {
      :accept_suggestion "<M-l>"
      :clear_suggestion "<M-[>"
      :accept_word "<M-]>"
    }
})

; (map! 
;   :mode :n
;   :key "<Esc>"
;   :command "<Esc>"
;   :options {:desc "Escape"}
; )
