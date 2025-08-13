;; Split function utility
(local split 
  (fn [inputstr sep]
    (let [sep (or sep "%s")  ; default to whitespace
          t []]
      (each [str (string.gmatch inputstr (.. "([^" sep "]+)"))]
        (table.insert t str))
      t)))

(local autocmd vim.api.nvim_create_autocmd)

;; Restore cursor position on file open
(autocmd "BufReadPost"
  {:pattern "*"
   :callback (fn []
               (let [line (vim.fn.line "'\"")]
                 (when (and (> line 1)
                           (<= line (vim.fn.line "$"))
                           (not= vim.bo.filetype "commit")
                           (= (vim.fn.index ["xxd" "gitrebase"] vim.bo.filetype) -1))
                   (vim.cmd "normal! g`\""))))})

;; Enable autoformat by default
(set vim.g.autoformat_enabled true)

;; Auto-format on save
(autocmd ["BufWritePre"]
  {:group (vim.api.nvim_create_augroup "NvchadWritePre" {})
   :callback (fn [ev]
               (let [ft-to-lsp {:javascript "vtsls"
                               :typescript "vtsls"
                               :lua "luals"
                               :html "htmlls"
                               :eruby "htmlls"
                               :python "pyright"
                               :go "gopls"}
                     ft (. vim.bo ev.buf :filetype)
                     preferred-client (. ft-to-lsp ft)]
                 (when vim.g.autoformat_enabled
                   (let [buf ev.buf]
                     (vim.lsp.buf.format
                       {:bufnr buf
                        :filter (fn [client]
                                  (if preferred-client
                                      (do
                                        (vim.notify (.. "Formatting with " preferred-client))
                                        (= client.name preferred-client))
                                      (let [is-formattable (client:supports_method "textDocument/formatting")]
                                        (when is-formattable
                                          (vim.notify (.. "Formatting! with " client.name)))
                                        is-formattable)))})))))})

;; Quickfix filtering keymaps
(autocmd "FileType"
  {:pattern "qf"
   :callback (fn []
               (let [map vim.keymap.set]
                 ;; Filter quickfix
                 (map "n" "<localleader>f"
                      (fn []
                        (let [filter (vim.fn.input {:prompt "Filter: "})]
                          (when (and filter (not= filter ""))
                            (vim.cmd (.. "Cfilter " filter)))))
                      {:desc "Filter quickfix" :buffer true})
                 
                 ;; Exclude from quickfix
                 (map "n" "<localleader>F"
                      (fn []
                        (let [filter (vim.fn.input {:prompt "Exclude: "})]
                          (when (and filter (not= filter ""))
                            (vim.cmd (.. "Cfilter! " filter)))))
                      {:desc "Filter quickfix (exclude)" :buffer true})
                 
                 ;; Exclude current line from quickfix
                 (map "n" "<localleader>e"
                      (fn []
                        (let [line (vim.fn.getline ".")
                              part (. (split line "|") 3)]
                          (vim.cmd (.. "Cfilter! " part))))
                      {:desc "Filter quickfix (exclude)" :buffer true})))})

;; LSP attach keymaps
(autocmd "LspAttach"
  {:group (vim.api.nvim_create_augroup "DiagLspAttach" {})
   :callback (fn [ev]
               (let [map vim.keymap.set
                     buf ev.buf]
                 ;; Go to definition
                 (map "n" "gd"
                      (fn [] (vim.lsp.buf.definition))
                      {:desc "Go to definition" :buffer buf})
                 
                 ;; Workspace folder management
                 (map "n" "<leader>wa"
                      (fn [] (vim.lsp.buf.add_workspace_folder))
                      {:desc "Add Workspace Folder"})
                 
                 (map "n" "<leader>wl"
                      (fn [] (print (vim.inspect (vim.lsp.buf.list_workspace_folders))))
                      {:desc "List Workspace Folders"})
                 
                 (map "n" "<leader>wr"
                      (fn [] (vim.lsp.buf.remove_workspace_folder))
                      {:desc "Remove Workspace Folder"})))})

;; Quit with 'q' for certain file types
(autocmd "FileType"
  {:group (vim.api.nvim_create_augroup "QuitWithQ" {})
   :pattern ["qf" "help" "neotest-output"]
   :callback (fn [args]
               (let [map vim.keymap.set]
                 (map "n" "q"
                      (fn [] (vim.cmd "q"))
                      {:buffer args.buf})))})

;; LSP completion setup
; (autocmd "LspAttach"
;   {:group (vim.api.nvim_create_augroup "LspCompletion" {})
;    :callback (fn [ev]
;                (let [client (vim.lsp.get_client_by_id ev.data.client_id)]
;                  (when client
;                    (when (client:supports_method "textDocument/completion")
;                      (set vim.opt.completeopt "menuone,menu,noinsert,fuzzy,popup,preview")
;                      (vim.lsp.completion.enable true client.id ev.buf {:autotrigger true})
;                      (vim.keymap.set "i" "<C-Space>"
;                                      (fn [] (vim.lsp.completion.get)))))))})

;; MiniFiles dotfile toggle
(vim.api.nvim_create_autocmd "User"
  {:pattern "MiniFilesBufferCreate"
   :callback (fn [args]
               (let [filter-show (fn [fs-entry] true)
                     filter-hide (fn [fs-entry] 
                                   (not (vim.startswith fs-entry.name ".")))
                     buf-id args.data.buf_id]
                 (set vim.g.show_mini_dotfiles true)
                 (local MiniFiles (require :mini.files))
                 (let [toggle-dotfiles (fn []
                                         (set vim.g.show_mini_dotfiles 
                                              (not vim.g.show_mini_dotfiles))
                                         (let [new-filter (if vim.g.show_mini_dotfiles 
                                                              filter-show 
                                                              filter-hide)]
                                           (MiniFiles.refresh {:content {:filter new-filter}})))]
                   (vim.keymap.set "n" "g." toggle-dotfiles {:buffer buf-id}))))})

;; User config group and yank highlighting
(local userconfig (vim.api.nvim_create_augroup "UserConfig" {}))

(vim.api.nvim_create_autocmd "TextYankPost"
  {:group userconfig
   :callback (fn []
               (vim.hl.on_yank {:higroup "IncSearch" :timeout 400}))})

