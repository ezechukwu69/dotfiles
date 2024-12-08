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
        xAI = {
          __inherited_from = "openai",
          endpoint = "https://api.x.ai/v1",
          model = "grok-beta",
          api_key_name = "XAI_API_KEY",
        }
      },
      behaviour = {
        auto_suggestions = true,
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
