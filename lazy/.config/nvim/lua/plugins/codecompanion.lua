return {
  -- "olimorris/codecompanion.nvim",
  --
  dir = "/home/ezechukwu69/development/lua/codecompanion.nvim",
  dev = true,
  enabled = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "hrsh7th/nvim-cmp", -- Optional: For using slash commands and variables in the chat buffer
    {
      "stevearc/dressing.nvim", -- Optional: Improves the default Neovim UI
      opts = {},
    },
    "nvim-telescope/telescope.nvim", -- Optional: For using slash commands
  },
  keys = {
    { "<leader>aa", "<cmd>CodeCompanionChat<CR>", desc = "Code companion chat" },
    { "<leader>at", "<cmd>CodeCompanionChat Toggle<CR>", desc = "Code companion toggle" },
    { "ga", "<cmd>CodeCompanionChat Add<CR>", desc = "Code companion toggle" },
    { "<leader>ac", "<cmd>CodeCompanionActions<CR>", desc = "Code companion actions" },
  },
  config = function()
    local adapters = require("codecompanion.adapters")
    -- llama-3.2-3b-instruct
    require("codecompanion").setup({
      strategies = {
        chat = {
          adapter = "gemini",
        },
        inline = {
          adapter = "gemini",
        },
        agent = {
          adapter = "gemini",
        },
      },
      adapters = {
        llama3 = function()
          return adapters.extend("openai_compatible", {
            name = "llama3",
            env = {
              url = "http://127.0.0.1:11434",
              api_key = "keys",
              chat_url = "/v1/chat/completions",
            },
            schema = {
              model = {
                default = "llama-3.2-3b-instruct",
              },
              num_ctx = {
                default = 4096,
              },
            },
          })
          -- return get_custom_adapter("http://127.0.0.1:11434")
        end,
      },
    })
  end,
}
