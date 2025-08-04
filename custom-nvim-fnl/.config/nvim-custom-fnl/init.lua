_G.nvim_start_time = vim.loop.hrtime()
vim.pack.add({
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvimdev/dashboard-nvim",
  "https://github.com/rktjmp/hotpot.nvim",
  "https://github.com/udayvir-singh/hibiscus.nvim"
})

require("hotpot").setup({
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true
})

require("custom-dashboard")
require("options")
require("mappings")
require("lsp")
require("plugins")
require("user_commands")
require("autocommands")
