return {
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    -- branch = "cmp_blink_compat",
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
      mode = "agentic",
      enable_cursor_planning_mode = nil,
      disabled_tools = {
        "read_file",
        "rename_file",
        "rename_dir",
        "list_files",
        "bash",
        "delete_file",
        "delete_dir",
        "search_files",
      },
      system_prompt = function()
        local hub = require("mcphub").get_hub_instance()
        return hub:get_active_servers_prompt()
      end,
      custom_tools = function()
        return {
          require("mcphub.extensions.avante").mcp_tool(),
        }
      end,
      gemini = {
        model = "gemini-2.5-flash",
      },
      providers = {
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
        auto_suggestions = false, -- Experimental stage
        auto_set_highlight_group = true,
        auto_set_keymaps = true,
        auto_apply_diff_after_generation = false,
        support_paste_from_clipboard = true,
        minimize_diff = true, -- Whether to remove unchanged lines when applying a code block,
        -- enable_token_counting = true, -- Whether to enable Cursor Planning Mode. Default to false.
      },
      mappings = {
        --- @class AvanteConflictMappings
        diff = {
          ours = "co",
          theirs = "ct",
          all_theirs = "ca",
          both = "cb",
          cursor = "cc",
          next = "]x",
          prev = "[x",
        },
        suggestion = {
          accept = "<M-l>",
          next = "<M-]>",
          prev = "<M-[>",
          dismiss = "<C-]>",
        },
        jump = {
          next = "]]",
          prev = "[[",
        },
        submit = {
          normal = "<CR>",
          insert = "<C-s>",
        },
        sidebar = {
          apply_all = "A",
          apply_cursor = "a",
          switch_windows = "<Tab>",
          reverse_switch_windows = "<S-Tab>",
        },
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
      web_search_engine = {
        provider = "tavily",
      },
      diff = {
        list_opener = "copen",
        override_timeoutlen = 500,
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
