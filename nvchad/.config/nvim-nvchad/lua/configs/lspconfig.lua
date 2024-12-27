-- load defaults i.e lua_lsp
require("mason").setup()
require("mason-nvim-dap").setup {
  automatic_installation = true,
  handlers = {
    function(config)
      require("mason-nvim-dap").default_setup(config)
    end,
  },
}

local nvlsp = require "nvchad.configs.lspconfig"

require("mason-lspconfig").setup {
  handlers = {
    function(server)
      require("lspconfig")[server].setup {
        on_attach = nvlsp.on_attach,
        on_init = nvlsp.on_init,
        capabilities = nvlsp.capabilities,
      }
    end,
  },
}
require("nvchad.configs.lspconfig").defaults()
-- After setting up mason-lspconfig you may set up servers via lspconfig
-- require("lspconfig").lua_ls.setup {}
-- require("lspconfig").rust_analyzer.setup {}
-- ...
-- local lspconfig = require "lspconfig"
--
-- -- EXAMPLE
-- local servers = { "html", "cssls", "vtsls", "intelephense" }
--
-- -- lsps with default config
-- for _, lsp in ipairs(servers) do
--   lspconfig[lsp].setup {
--     on_attach = function(client, bufnr)
--       nvlsp.on_attach(client, bufnr)
--     end,
--     on_init = nvlsp.on_init,
--     capabilities = nvlsp.capabilities,
--   }
-- end

-- configuring single server, example: typescript
-- lspconfig.ts_ls.setup {
--   on_attach = nvlsp.on_attach,
--   on_init = nvlsp.on_init,
--   capabilities = nvlsp.capabilities,
-- }
