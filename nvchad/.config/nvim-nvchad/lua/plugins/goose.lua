return {
  "azorng/goose.nvim",
  enabled = false,
  config = function()
    require("goose").setup({
      default_global_keymaps = true, -- If false, disables all default global keymaps
      keymap = {
        global = {
          toggle = "<leader>ig", -- Open goose. Close if opened
          open_input = "<leader>ii", -- Opens and focuses on input window on insert mode
          open_input_new_session = "<leader>iI", -- Opens and focuses on input window on insert mode. Creates a new session
          open_output = "<leader>io", -- Opens and focuses on output window
          toggle_focus = "<leader>it", -- Toggle focus between goose and last window
          close = "<leader>iq", -- Close UI windows
          toggle_fullscreen = "<leader>if", -- Toggle between normal and fullscreen mode
          select_session = "<leader>is", -- Select and load a goose session
          goose_mode_chat = "<leader>imc", -- Set goose mode to `chat`. (Tool calling disabled. No editor context besides selections)
          goose_mode_auto = "<leader>ima", -- Set goose mode to `auto`. (Default mode with full agent capabilities)
          configure_provider = "<leader>ip", -- Quick provider and model switch from predefined list
          diff_open = "<leader>id", -- Opens a diff tab of a modified file since the last goose prompt
          diff_next = "<leader>i]", -- Navigate to next file diff
          diff_prev = "<leader>i[", -- Navigate to previous file diff
          diff_close = "<leader>ic", -- Close diff view tab and return to normal editing
          diff_revert_all = "<leader>ira", -- Revert all file changes since the last goose prompt
          diff_revert_this = "<leader>irt", -- Revert current file changes since the last goose prompt
        },
        window = {
          submit = "<cr>", -- Submit prompt
          close = "<esc>", -- Close UI windows
          stop = "<C-c>", -- Stop goose while it is running
          next_message = "]]", -- Navigate to next message in the conversation
          prev_message = "[[", -- Navigate to previous message in the conversation
          mention_file = "@", -- Pick a file and add to context. See File Mentions section
          toggle_pane = "<tab>", -- Toggle between input and output panes
          prev_prompt_history = "<up>", -- Navigate to previous prompt in history
          next_prompt_history = "<down>", -- Navigate to next prompt in history
        },
      },
      ui = {
        window_width = 0.35, -- Width as percentage of editor width
        input_height = 0.15, -- Input height as percentage of window height
        fullscreen = false, -- Start in fullscreen mode (default: false)
        layout = "right", -- Options: "center" or "right"
        floating_height = 0.8, -- Height as percentage of editor height for "center" layout
        display_model = true, -- Display model name on top winbar
        display_goose_mode = true, -- Display mode on top winbar: auto|chat
      },
      providers = {
        --[[
    Define available providers and their models for quick model switching
    anthropic|azure|bedrock|databricks|google|groq|ollama|openai|openrouter
    Example:
    openrouter = {
      "anthropic/claude-3.5-sonnet",
      "openai/gpt-4.1",
    },
    ollama = {
      "cogito:14b"
    }
    --]]
        google = {
          "gemini-2.5-pro-preview-05-06",
          "gemini-2.5-flash-preview-04-17",
        },
      },
    })
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "MeanderingProgrammer/render-markdown.nvim",
      opts = {
        anti_conceal = { enabled = false },
      },
    },
  },
}
