(vim.api.nvim_create_user_command :LspRestart
  (fn []
    (local bufnr (vim.api.nvim_get_current_buf))
    (each [_ client (pairs (vim.lsp.get_clients {:bufnr bufnr}))]
      (client.stop client true))
    (vim.defer_fn (fn []
                    (vim.cmd.edit)) 100))
  {})

(local map (fn [tbl func]
  (local result {})
  (each [i v (ipairs tbl)]
    (tset result i (func v i)))
  result))

(vim.api.nvim_create_user_command :PackDelete
  (fn []
    (vim.ui.select (map (vim.pack.get) (fn [x] x.spec.name))
      {:prompt "Select package to uninstall"}
      (fn [item]
        (vim.pack.del [item]))))
  {})
