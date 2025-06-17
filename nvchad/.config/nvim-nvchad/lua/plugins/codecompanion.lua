return {
  {
    "olimorris/codecompanion.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "ravitemer/mcphub.nvim",
      {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = { "markdown", "codecompanion" }
      },
      {
        "echasnovski/mini.diff",
        config = function()
          local diff = require("mini.diff")
          diff.setup({
            -- Disabled by default
            source = diff.gen_source.none(),
          })
        end,
      },
      {
        "HakonHarnes/img-clip.nvim",
        opts = {
          filetypes = {
            codecompanion = {
              prompt_for_file_name = false,
              template = "[Image]($FILE_PATH)",
              use_absolute_path = true,
            },
          },
        },
      },
    },
    keys = {
      { "<leader>aa", "<cmd>CodeCompanionChat<cr>",   desc = "CodeCompanion Chat",   mode = { "n" } },
      { "<leader>at", "<cmd>CodeCompanionAction<cr>", desc = "CodeCompanion Action", mode = "n" },
      { "<leader>ac", "<cmd>CodeCompanion<cr>",       desc = "CodeCompanion",        mode = { "n", "v" } },
    },
    opts = {
      extensions = {
        mcphub = {
          callback = "mcphub.extensions.codecompanion",
          opts = {
            make_vars = true,
            make_slash_commands = true,
            show_result_in_chat = false,
          }
        }
      },
      adapters = {
        opts = {
          show_model_choices = true
        }
      },
      strategies = {
        chat = {
          tools = {
            opts = {
              collapse_tools = true,
              default_tools = {
                "agentic_mode"
              }
            },
            groups = {
              ["agentic_mode"] = {
                description = "Agentic Mode",
                system =
                "You are an AI assistant that helps me to write code using tools provided to you, make sure to write clean and readable code",
                tools = {
                  "cmd_runner",
                  "create_file",
                  "file_search",
                  "grep_search",
                  "insert_edit_into_file",
                  "next_edit_suggestion",
                  "read_file",
                  "web_search"
                },
                opts = {
                  collapse_tools = true,
                }
              }
            }
          },
          adapter = "gemini",
          model = "gemini-2.5-flash"
        },
        inline = {
          adapter = "gemini",
          model = "gemini-2.5-flash"
        },
        cmd = {
          adapter = "gemini",
          model = "gemini-2.5-flash"
        }
      }
    },
  },
}
