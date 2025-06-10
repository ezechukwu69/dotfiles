return {
  "udayvir-singh/tangerine.nvim",
  lazy = false,
  dependencies = { "udayvir-singh/hibiscus.nvim" },
  config = function()
    require 'tangerine'.setup {
      target = vim.fn.stdpath [[data]] .. "/tangerine",

      -- compile files in &rtp
      rtpdirs = {
        "ftplugin",
      },

      compiler = {
        -- disable popup showing compiled files
        verbose = false,

        -- compile every time changes are made to fennel files or on entering vim
        hooks = { "onsave", "oninit" }
      },
    }
  end,
}
