return {
  { "vim-test/vim-test" },
  { "nvim-neotest/neotest-vim-test" },
  {
    "nvim-neotest/neotest",
    keys = {
      {
        "<leader>ntt",
        function()
          require("neotest").run.run()
        end,
        mode = "n",
        desc = "Run Neotest"
      },
      {
        "<leader>ntr",
        function()
          require("neotest").run.run(vim.fn.expand("%"))
        end
        ,
        mode = "n",
        desc = "Run Neotest on current file"
      },
      {
        "<leader>ntd",
        function()
          require("neotest").run.run({ strategy = "dap" })
        end,
        mode = "n",
        desc = "Run Neotest on current file with dap"
      },
      {
        "<leader>nto",
        function()
          require("neotest").output.open({ enter = true })
        end,
        mode = "n",
        desc = "Open Neotest output"
      },
      {
        "<leader>ntp",
        function()
          require("neotest").output_panel.toggle()
        end,
        mode = "n",
        desc = "Toggle Neotest output panel"
      },
      {
        "<leader>nts",
        function()
          require("neotest").summary.toggle()
        end,
        mode = "n",
        desc = "Toggle Neotest summary"
      }
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-vim-test"),
        },
      })
    end
  }
}
