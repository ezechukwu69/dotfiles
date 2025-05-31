require("nvchad.configs.lspconfig").defaults()

local servers = {
  html = {},
  awk_ls = {},
  bashls = {},
  vtsls = {},

  pyright = {
    settings = {
      python = {
        analysis = {
          autoSearchPaths = true,
          typeCheckingMode = "basic",
        },
      },
    },
  },
}

for name, opts in pairs(servers) do
  vim.lsp.enable(name)  -- nvim v0.11.0 or above required
  vim.lsp.config(name, opts) -- nvim v0.11.0 or above required
end

-- read :h vim.lsp.config for changing options of lsp servers 
