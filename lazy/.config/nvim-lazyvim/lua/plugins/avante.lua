return {
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    -- branch = "cmp_blink_compat",
    build = "make",
    -- enabled = false,
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
      provider = "qwen_local",
      auto_suggestions_provider = "gemini",
      vendors = {
        xAI = {
          __inherited_from = "openai",
          endpoint = "https://api.x.ai/v1",
          model = "grok-beta",
          api_key_name = "XAI_API_KEY",
        },

        qwen_local = {
          __inherited_from = "openai",
          endpoint = "http://localhost:1234/v1",
          model = "Qwen2.5-Coder-7B-Instruct-GGUF/Qwen2.5-Coder-7B-Instruct-Q4_K_M.gguf",
          api_key_name = "XAI_API_KEY",
        },
        groq = {
          __inherited_from = "openai",
          endpoint = "https://api.groq.com/openai/v1",
          model = "gemma2-9b-it",
          api_key_name = "GROQ_API_KEY",
        },
        ["genimi2-experimental"] = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          model = "google/gemini-2.0-flash-exp:free",
          api_key_name = "OPENROUTER_API_KEY",
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
      file_selector = {
        --- @alias FileSelectorProvider "native" | "fzf" | "telescope" | string
        provider = "fzf",
        -- Options override for custom providers
        provider_opts = {},
      },
    },
  },
}
