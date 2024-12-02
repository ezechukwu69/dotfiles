return {
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    build = "make",
    enabled = true,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
      {
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = { insert_mode = true },
            use_absolute_path = true,
          },
        },
      },
      {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = { "markdown", "Avante" },
        opts = { file_types = { "markdown", "Avante" } },
      },
    },
    opts = {
      provider = "gemini",
      auto_suggestions_provider = "gemini",
      vendors = {
        ollama = {
          ["local"] = true,
          endpoint = "127.0.0.1:11434/v1",
          model = "llama-3.2-3b-instruct",
          parse_curl_args = function(opts, code_opts)
            return {
              url = opts.endpoint .. "/chat/completions",
              headers = {
                Accept = "application/json",
                ["Content-Type"] = "application/json",
              },
              body = {
                model = opts.model,
                messages = require("avante.providers").copilot.parse_message(code_opts),
                max_tokens = 2048,
                stream = true,
              },
            }
          end,
          parse_response_data = function(data_stream, event_state, opts)
            require("avante.providers").openai.parse_response(data_stream, event_state, opts)
          end,
        },
      },
      behaviour = {
        auto_suggestions = false,
        auto_apply_diff_after_generation = true,
        support_paste_from_clipboard = true,
      },
      windows = {
        sidebar_header = {
          enabled = true,
          align = "center",
          rounded = true,
        },
        ask = {
          focus_on_apply = "theirs",
        },
      },
    },
  },
}
