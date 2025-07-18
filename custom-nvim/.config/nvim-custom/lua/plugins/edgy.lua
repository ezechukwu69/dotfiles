-- dapui_stacks
-- dapui_breakpoints
-- dapui_scopes
-- dapui_watches
-- dap-repl
-- dapui_console
return {
  "folke/edgy.nvim",
  opts = {
    options = {
      left = { size = 40 },
      right = { size = 15 },
      bottom = { size = 15 },
      top = { size = 15 },
    },
    left = {
      {
        title = "DapUI stacks",
        ft = "dapui_stacks",
      },
      {
        title = "DapUI breakpoints",
        ft = "dapui_breakpoints",
      },
      {
        title = "DapUI watches",
        ft = "dapui_watches",
      },
      {
        title = "DapUI console",
        ft = "dapui_console",
      },
    },
    bottom = {
      {
        title = "Dap-REPL",
        ft = "dap-repl",
      },
      {
        title = "DapUI scopes",
        ft = "dapui_scopes",
      },
      {
        title = "Quickfix List",
        ft = "qf",
      },
    },


    -- table.insert(opts.right, {
    --   title = "Avante",
    --   ft = "Avante",
    --   size = {
    --     height = 1,
    --   },
    -- })
    -- table.insert(opts.right, {
    --   title = "Avante Selected Files",
    --   ft = "AvanteSelectedFiles",
    --   -- size = {
    --   --   height = 3,
    --   -- },
    -- })
    -- table.insert(opts.right, {
    --   title = "Avante Input",
    --   ft = "AvanteInput",
    --   -- size = {
    --   --   height = 3,
    --   -- },
    -- })
    -- table.insert(opts.right, {
    --   title = "CodeCompanion",
    --   ft = "codecompanion",
    -- })
  },
}
