return {
  "olimorris/codecompanion.nvim",
  -- dir = "/home/ezechukwu69/development/lua/codecompanion.nvim",
  -- dev = true,
  enabled = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "hrsh7th/nvim-cmp",         -- Optional: For using slash commands and variables in the chat buffer
    {
      "stevearc/dressing.nvim", -- Optional: Improves the default Neovim UI
      opts = {},
    },
  },
  keys = {
    { "<leader>aa", "<cmd>CodeCompanionActions<CR>",     mode = { "n", "v" },             desc = "Code companion actions" },
    { "<leader>at", "<cmd>CodeCompanionChat Toggle<CR>", desc = "Code companion toggle" },
    { "<leader>an", "<cmd>CodeCompanionChat Add<CR>",    desc = "Code companion add chat" },
    -- { "<leader>ac", "<cmd>CodeCompanionActions<CR>", mode = { "n", "v" }, desc = "Code companion actions" },
  },
  config = function()
    local adapters = require("codecompanion.adapters")
    -- llama-3.2-3b-instruct
    require("codecompanion").setup({
      display = {
        diff = {
          provider = "mini_diff",
        },
      },
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
