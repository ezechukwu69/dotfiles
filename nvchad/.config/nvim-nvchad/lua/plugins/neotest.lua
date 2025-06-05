return {
  {
    "nvim-neotest/neotest",
    event = "VeryLazy",
    dependencies = {
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-jest",
      "nvim-neotest/neotest-plenary",
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "nvim-neotest/neotest-vim-test",
      "vim-test/vim-test",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter"
    },
    keys = {
      {
        "<leader>bt",
        function() require("neotest").run.run() end,
        desc = "Run tests"
      },
      {
        "<leader>bw",
        function() require("neotest").watch.toggle(vim.fn.expand("%")) end,
        desc = "watch tests in file"
      },
      {
        "<leader>bf",
        function() require("neotest").run.run(vim.fn.expand("%")) end,
        desc = "Run tests in file"
      },
      {
        "<leader>bd",
        function() require("neotest").run.run({ strategy = "dap" }) end,
        desc = "Debug tests in file"
      },
      {
        "<leader>bs",
        function() require("neotest").run.stop() end,
        desc = "Stop tests"
      },
      {
        "<leader>bS",
        function() require("neotest").summary.toggle() end,
        desc = "Toggle test summary"
      },
      {
        "<leader>bo",
        function() require("neotest").output.open({enter = true}) end,
        desc = "Toggle test summary"
      },
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-python")({
            dap = { justMyCode = false },
          }),
          require("neotest-jest")({}),
          require("neotest-plenary"),
          require("neotest-vim-test")({
            ignore_file_types = { "vim", "lua" },
          }),
        },
      })
    end
  }
}
