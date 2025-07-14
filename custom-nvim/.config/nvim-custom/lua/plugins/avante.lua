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
      "ravitemer/mcphub.nvim",
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
      auto_suggestion_provider = "gemini",
      mode = "agentic",
      enable_cursor_planning_mode = nil,
      system_prompt = function()
        local hub = require("mcphub").get_hub_instance()
        return hub and hub:get_active_servers_prompt() or ""
      end,
      custom_tools = function()
        return {
          require("mcphub.extensions.avante").mcp_tool(),
        }
      end,
      providers = {
        gemini = {
          model = "gemini-2.5-flash",
        },
        ["genimi2-experimental"] = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          model = "google/gemini-2.0-flash-exp:free",
          api_key_name = "OPENROUTER_API_KEY",
        },
      },
      behaviour = {
        auto_suggestion = false,
        support_paste_from_clipboard = true,
        enable_token_counting = true, -- Whether to enable Cursor Planning Mode. Default to false.
      },
      web_search_engine = {
        provider = "tavily",
      },
      selector = {
        provider = "snacks",
        -- Options override for custom providers
        provider_opts = {},
      },
    },
    config = function(_, opts)
      require("avante").setup(opts)
    end,
  },
}
