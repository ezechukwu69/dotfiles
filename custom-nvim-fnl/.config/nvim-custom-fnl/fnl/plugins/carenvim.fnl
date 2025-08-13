(vim.pack.add [
               {:src "https://github.com/saghen/blink.cmp" :version (vim.version.range "1.*") }
               "https://github.com/rafamadriz/friendly-snippets"
               ])

(local blink (require :blink.cmp))

(blink.setup {
             :keymap {
                      :preset :enter
             }
            
             :appearance {
                      :nerd_font_variant "normal"
             }

             :completion { :documentation {  :auto_show true  }  }
})
