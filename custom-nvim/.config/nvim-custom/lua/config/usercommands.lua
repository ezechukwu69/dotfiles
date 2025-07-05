vim.api.nvim_create_user_command('LspRestart', function()
  for _, client in pairs(vim.lsp.get_clients()) do
    client.stop(client, true)
  end
  vim.cmd('edit')
end, {})
