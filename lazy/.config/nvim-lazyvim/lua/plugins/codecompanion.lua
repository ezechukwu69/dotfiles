local function configure_keymaps()
  vim.api.nvim_set_keymap("n", "<leader>it", "<cmd>CodeCompanionActions<cr>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("n", "<leader>ii", "<cmd>CodeCompanion<cr>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("v", "<leader>it", "<cmd>CodeCompanionActions<cr>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("n", "<leader>ia", "<cmd>CodeCompanionChat Toggle<cr>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("v", "<leader>ia", "<cmd>CodeCompanionChat Toggle<cr>", { noremap = true, silent = true })
  vim.api.nvim_set_keymap("v", "<leader>ic", "<cmd>CodeCompanionChat Add<cr>", { noremap = true, silent = true })

  -- Expand 'cc' into 'CodeCompanion' in the command line
  vim.cmd([[cab cc CodeCompanion]])
end

local function configure_opts()
  local opts = {
    display = {
      chat = {
        window = {
          width = 0.3,
        },
        show_header_separator = true, -- Show header separators in the chat buffer? Set this to false if you're using an exteral markdown formatting plugin
        separator = "─", -- The separator between the different messages in the chat buffer
        show_settings = true, -- Show the settings button in the chat buffer?
      },
      diff = {
        enabled = true,
        provider = "mini_diff",
      },
    },
    opts = {
      log_level = "DEBUG",
    },
    extensions = {
      mcphub = {
        callback = "mcphub.extensions.codecompanion",
        opts = {
          show_result_in_chat = true, -- Show the mcp tool result in the chat buffer
          make_vars = true, -- make chat #variables from MCP server resources
          make_slash_commands = true, -- make /slash_commands from MCP server prompts
        },
      },
    },
    strategies = {
      chat = {
        adapter = "gemini2",
        slash_commands = {
          ["buffer"] = {
            callback = "strategies.chat.slash_commands.buffer",
            description = "Insert open buffers",
            opts = {
              contains_code = true,
              -- provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              provider = "snacks", -- default|telescope|mini_pick|fzf_lua
            },
          },
          ["file"] = {
            callback = "strategies.chat.slash_commands.file",
            description = "Insert a file",
            opts = {
              contains_code = true,
              max_lines = 1000,
              -- provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              provider = "snacks", -- default|telescope|mini_pick|fzf_lua
            },
          },
          ["help"] = {
            callback = "strategies.chat.slash_commands.help",
            description = "Insert content from help tags",
            opts = {
              contains_code = false,
              max_lines = 128, -- Maximum amount of lines to of the help file to send (NOTE: each vimdoc line is typically 10 tokens)
              -- provider = "fzf_lua", -- telescope|mini_pick|fzf_lua
              provider = "snacks", -- default|telescope|mini_pick|fzf_lua
            },
          },
          ["symbols"] = {
            callback = "strategies.chat.slash_commands.symbols",
            description = "Insert symbols for a selected file",
            opts = {
              contains_code = true,
              -- provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              provider = "snacks", -- default|telescope|mini_pick|fzf_lua
            },
          },
        },
      },
      inline = {
        adapter = "gemini2",
      },
    },
    adapters = {
      ["gemini"] = function()
        return require("codecompanion.adapters").extend("gemini", {
          env = {
            api_key = "GEMINI_API_KEY",
            model = "schema.model.default",
          },
          schema = {
            model = {
              default = "gemini-1.5-flash",
            },
          },
        })
      end,
      gemini2 = function()
        return require("codecompanion.adapters").extend("gemini", {
          env = {
            api_key = "GEMINI_API_KEY",
            model = "schema.model.default",
          },
          schema = {
            model = {
              default = "gemini-2.5-flash-preview-04-17",
            },
          },
        })
      end,
    },
  }
  return opts
end

return {
  "olimorris/codecompanion.nvim",
  event = "VeryLazy",
  enabled = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    { "MeanderingProgrammer/render-markdown.nvim", ft = { "markdown", "codecompanion" } },
  },
  opts = function()
    return configure_opts()
  end,
  config = function(_, opts)
    require("codecompanion").setup(opts)
    configure_keymaps()
  end,
}
