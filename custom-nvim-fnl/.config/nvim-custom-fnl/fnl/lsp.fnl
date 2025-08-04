(vim.lsp.enable [
                 :fennel_language_server 
                 :luals
                 :vtsls
                 :emmet_ls
                 :rubylsp
                 :prismals
                 :htmlls
                 :cssls
                 :clangd
                 :neocmake
])

(vim.diagnostic.config {:signs {:text {(. vim.diagnostic.severity :ERROR) " "
                                       (. vim.diagnostic.severity :WARN) " "
                                       (. vim.diagnostic.severity :INFO) " "
                                       (. vim.diagnostic.severity :HINT) " "}
                                :numhl {(. vim.diagnostic.severity :ERROR) :DiagnosticSignError
                                        (. vim.diagnostic.severity :WARN) :DiagnosticSignWarn
                                        (. vim.diagnostic.severity :INFO) :DiagnosticSignInfo
                                        (. vim.diagnostic.severity :HINT) :DiagnosticSignHint}}
                        :virtual_lines {:current_line true}
                        :update_in_insert false
                        :float {:border :rounded :source true}
                        :severity_sort true})

(vim.lsp.inlay_hint.enable true)
