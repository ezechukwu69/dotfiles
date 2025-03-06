return {
  {
    "akinsho/flutter-tools.nvim",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim", -- optional for vim.ui.select
    },
    keys = {
      { "<localleader>fe", "<cmd>FlutterEmulators<cr>", desc = "Flutter Emulators", ft = "dart" },
      { "<localleader>fg", "<cmd>FlutterPubGet<cr>", desc = "Flutter Pub Get", ft = "dart" },
      { "<localleader>fs", "<cmd>FlutterRun<cr>", desc = "Flutter Run", ft = "dart" },
      { "<localleader>fr", "<cmd>FlutterReload<cr>", desc = "Flutter Reload", ft = "dart" },
      { "<localleader>fR", "<cmd>FlutterRestart<cr>", desc = "Flutter Restart", ft = "dart" },
    },
    opts = function(_, opts)
      return vim.tbl_deep_extend("force", opts, {
        decorations = {
          statusline = {
            device = true,
            app_version = true,
            project_config = true,
          },
        },
        debugger = {
          enabled = true,
          exception_breakpoints = {},
        },
        register_configurations = function(paths)
          require("dap").configurations.dart = {
            {
              name = "Flutter: Run",
              type = "dart",
              request = "launch",
            },
            {
              name = "Flutter: Run (Verbose)",
              type = "dart",
              request = "launch",
              args = {
                "-v",
              },
            },
          }
          require("dap.ext.vscode").load_launchjs()
        end,
        widget_guides = {
          enabled = true,
        },
        dev_log = {
          enabled = false,
          -- Open as split horizontal
          open_cmd = "split",
        },
      })
    end,
  },
}
