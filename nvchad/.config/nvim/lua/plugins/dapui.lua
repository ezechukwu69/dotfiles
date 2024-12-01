return {
  "rcarriga/nvim-dap-ui",
  dependencies = {
    "mfussenegger/nvim-dap",
    "nvim-neotest/nvim-nio",
  },
  event = "VeryLazy",
  config = function()
    local dapui = require "dapui"
    local dap = require "dap"

    dapui.setup {
      controls = {
        element = "repl",
        enabled = true,
        icons = {
          disconnect = "",
          pause = "",
          play = "",
          run_last = "",
          step_back = "",
          step_into = "",
          step_out = "",
          step_over = "",
          terminate = "",
        },
      },
      element_mappings = {},
      expand_lines = true,
      floating = {
        border = "single",
        mappings = {
          close = { "q", "<Esc>" },
        },
      },
      force_buffers = true,
      icons = {
        collapsed = "",
        current_frame = "",
        expanded = "",
      },
      layouts = {
        {
          elements = {
            {
              id = "scopes",
              size = 5,
            },
            {
              id = "breakpoints",
              size = 0.5,
            },
            -- {
            --   id = "stacks",
            --   size = 0.25,
            -- },
            -- {
            --   id = "watches",
            --   size = 0.25,
            -- },
          },
          position = "left",
          size = 30,
        },
        -- {
        --   elements = {
        --     {
        --       id = "repl",
        --       size = 0.5,
        --     },
        --     {
        --       id = "console",
        --       size = 0.5,
        --     },
        --   },
        --   position = "bottom",
        --   size = 6,
        -- },
      },
      mappings = {
        edit = "e",
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        repl = "r",
        toggle = "t",
      },
      render = {
        indent = 1,
        max_value_lines = 100,
      },
    }

    dap.listeners.before.attach.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
      dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
      dapui.close()
    end

    local which_key = require "which-key"
    which_key.add {
      { "<leader>d", group = "Dap / Diagnostics" },
      { "<leader>c", group = "Code" },
      {
        "<leader>dt",
        function()
          dapui.toggle "sidebar"
        end,
        desc = "Toggle Dap UI",
      },
      {
        "<leader>dr",
        function()
          dapui.float_element "repl"
        end,
        desc = "Open Dap Repl",
      },
      {
        "<leader>dC",
        function()
          dapui.float_element "console"
        end,
        desc = "Open Dap Console",
      },
      {
        "<leader>dw",
        function()
          dapui.float_element "watches"
        end,
        desc = "Open Dap Watches",
      },
      {
        "<leader>dc",
        function()
          dap.continue()
        end,
        desc = "Continue/Run Dap",
      },
      {
        "<leader>db",
        function()
          dap.toggle_breakpoint()
        end,
        desc = "Toggle Dap Breakpoint",
      },
      {
        "<leader>dB",
        function()
          dap.step_back()
        end,
        desc = "Dap Step Back",
      },
      {
        "<leader>dF",
        function()
          dap.step_into()
        end,
        desc = "Dap Step Into",
      },
      {
        "<leader>dO",
        function()
          dap.step_out()
        end,
        desc = "Dap Step Out",
      },
      {
        "<leader>dT",
        function()
          dap.terminate()
        end,
        desc = "Dap Terminate",
      },
      {
        "<leader>dR",
        function()
          dap.restart()
        end,
        desc = "Dap Restart",
      },
      {
        "<leader>dP",
        function()
          dap.pause()
        end,
        desc = "Dap Pause",
      },
    }
  end,
}
