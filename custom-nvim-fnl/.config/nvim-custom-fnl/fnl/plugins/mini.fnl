(vim.pack.add ["https://github.com/echasnovski/mini.nvim"])

(local pairs (require "mini.pairs"))
(pairs.setup)

(local ai (require :mini.ai))
(local spec_treesitter ai.gen_spec.treesitter)

(ai.setup
  {
   ;; Table with textobject id as fields, textobject specification as values.
   ;; Also use this to disable builtin textobjects. See |MiniAi.config|.
   :custom_textobjects
   {
    :f (spec_treesitter {:a ["@function.outer" "@method.outer"]
                         :i ["@function.inner" "@method.inner"]})
    :c (spec_treesitter {:a "@class.outer" :i "@class.inner"})
    :C (spec_treesitter {:a "@comment.outer" :i "@comment.inner"})
    :a (spec_treesitter {:a "@parameter.inner" :i "@parameter.inner"})
    :o (spec_treesitter {:a ["@conditional.outer" "@loop.outer"]
                         :i ["@conditional.inner" "@loop.inner"]})
    :p (spec_treesitter {:a ["@field.outer" "@property.outer"]
                         :i ["@field.inner" "@property.inner"]})
    :v (spec_treesitter {:a "@variable.outer" :i "@variable.inner"})}

   ;; Module mappings. Use `''` (empty string) to disable one.
   :mappings
   {
    ;; Main textobject prefixes
    :around "a"
    :inside "i"

    ;; Next/last variants
    :around_next "an"
    :inside_next "in"
    :around_last "al"
    :inside_last "il"

    ;; Move cursor to corresponding edge of `a` textobject
    :goto_left "g["
    :goto_right "g]"}

   ;; Number of lines within which textobject is searched
   :n_lines 50

   ;; How to search for object. One of 'cover', 'cover_or_next', etc.
   :search_method "cover_or_next"

   ;; Whether to disable showing non-error feedback
   :silent false})

;; Active plugin configurations
(local bufremove (require :mini.bufremove))
(bufremove.setup)

(local splitjoin (require :mini.splitjoin))
(splitjoin.setup
 {:mappings {:toggle "gJ"}})

(local surround (require :mini.surround))
(surround.setup
 {:mappings
  {:add            "sa" ; Add surrounding in Normal and Visual modes
   :delete         "sd" ; Delete surrounding
   :find           "sf" ; Find surrounding (to the right)
   :find_left      "sF" ; Find surrounding (to the left)
   :highlight      "sh" ; Highlight surrounding
   :replace        "sr" ; Replace surrounding
   :update_n_lines "sn" ; Update `n_lines`
   :suffix_last    "l"  ; Suffix to search with 'prev' method
   :suffix_next    "n"}}) ; Suffix to search with 'next' method

(local snippets (require :mini.snippets))
(snippets.setup {})

;; ---
;; Commented-out plugin configurations
;; ---

;; ((require :mini.files).setup
;;  {:mappings {}
;;   :options {:use_as_default_explorer true
;;             :permanently_delete false}
;;   :windows {:preview true
;;             :width_preview 70}})

;; ((require :mini.cursorword).setup)

;; ((require :mini.sessions).setup
;;  {:autoread true
;;   :autowrite true
;;   :direcrory (.. (vim.fn.stdpath "data") "/sessions")
;;   :file "Session.vim"
;;   :verbose {:read true :write true :delete true}})

;; ---
;; Mini.starter configuration (commented out)
;; ---

;; (local logo (vim.fn.system "toilet -f smmono9 -F border 'Ezechukwu69'"))
;; (local starter (require :mini.starter))
;; (local delta (/ (- (vim.loop.hrtime) _G.nvim_start_time) 1e6))

;; (starter.setup
;;  {:autoopen true
;;   :header logo
;;   :items [(starter.sections.builtin_actions)
;;           ; (starter.sections.recent_files 10 false)
;;           (starter.sections.recent_files 5 true)
;;           ; (starter.sections.pick)
;;           ; Use this if you set up 'mini.sessions'
;;           ; (starter.sections.sessions 5 true)
;;           ]
;;   :content_hooks [(starter.gen_hook.adding_bullet)
;;                   (starter.gen_hook.aligning :center :center)
;;                   (starter.gen_hook.padding 3 2)]
;;   :footer (string.format "Loaded in %.2f ms" delta)})
