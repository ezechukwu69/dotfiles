-- dapui_stacks
-- dapui_breakpoints
-- dapui_scopes
-- dapui_watches
-- dap-repl
-- dapui_console
return {
  "folke/edgy.nvim",
  opts = function(_, opts)
    -- options =
    --   {
    --     left = { size = 30 },
    --     bottom = { size = 10 },
    --     top = { size = 10 },
    --   },
    opts.options = {
      left = { size = 25 },
      right = { size = 15 },
      bottom = { size = 15 },
      top = { size = 15 },
    }
    table.insert(opts.left, {
      title = "DapUI stacks",
      ft = "dapui_stacks",
    })
    table.insert(opts.left, {
      title = "DapUI breakpoints",
      ft = "dapui_breakpoints",
    })
    table.insert(opts.left, {
      title = "DapUI watches",
      ft = "dapui_watches",
    })
    table.insert(opts.bottom, {
      title = "Dap-REPL",
      ft = "dap-repl",
    })
    table.insert(opts.bottom, {
      title = "DapUI scopes",
      ft = "dapui_scopes",
    })
    table.insert(opts.left, {
      title = "DapUI console",
      ft = "dapui_console",
    })

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
  end,
}
