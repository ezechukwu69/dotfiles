return {
  {
    'akinsho/flutter-tools.nvim',
    lazy = false,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'stevearc/dressing.nvim', -- optional for vim.ui.select
    },
    opts = {
      decorations = {
        statusline = {
          device = true,
          app_version = true,
          project_config = true,
        },
      },
      debugger = {
        enabled = true,
      },
      register_configurations = function(paths)
        require('dap').configurations.dart = {
          {
            name = 'Flutter: Run',
            type = 'dart',
            request = 'launch',
          },
          {
            name = 'Flutter: Run (Verbose)',
            type = 'dart',
            request = 'launch',
            args = {
              '-v',
            },
          },
        }
        require('dap.ext.vscode').load_launchjs()
      end,
      widget_guides = {
        enabled = true,
      },
      dev_log = {
        enabled = false,
        -- Open as split horizontal
        open_cmd = 'split',
      },
    },
  },
}
