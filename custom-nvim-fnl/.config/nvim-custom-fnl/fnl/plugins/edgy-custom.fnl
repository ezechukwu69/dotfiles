(vim.pack.add [ "https://github.com/folke/edgy.nvim" ])

(local edgy (require :edgy))

(edgy.setup {
          :options {
               :left {:size 40}
               :right {:size 15}
               :bottom {:size 15}
               :top {:size 15}
          }
          :left [
             {
                :title "DapUI stack"
                :ft  :dapui_stacks
             }
             {
                :title "DapUI breakpoints"
                :ft  :dapui_breakpoints
             }
             {
                :title "DapUI watches"
                :ft  :dapui_watches
             }
             {
                :title "DapUI console"
                :ft  :dapui_console
             }
          ]
          :bottom [
                   {
                      :title "Dap-REPL"
                      :ft  :dap-repl
                   }
                   {
                      :title "DapUI scopes"
                      :ft  :dapui_scopes
                   }
                   {
                      :title "Quickfix List"
                      :ft  :qf
                   }
                   {
                      :title "Location List"
                      :ft  :loclist
                   }
                   {
                      :title "Help"
                      :ft  :help
                   }
          ]
})
