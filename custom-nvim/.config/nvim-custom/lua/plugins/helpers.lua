return {
  { "stevearc/dressing.nvim" },
  { "nvim-lua/plenary.nvim" },
  { "nvim-neotest/nvim-nio" },
  { "MunifTanjim/nui.nvim" },
  { "nvim-tree/nvim-web-devicons" },
  { 'L3MON4D3/LuaSnip',           version = vim.version.range("v2.*"), event = "InsertEnter" },
  {
    "HakonHarnes/img-clip.nvim",
    ft = { "markdown", "Avante" },
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
  {
    "A7Lavinraj/fyler.nvim",
    version = "stable",
    keys = {
      {
        "-",
        "<cmd>Fyler kind=float<cr>",
        desc = "Fyler",
        mode = "n"
      }
    },
    opts = {
      close_on_select = true,
      icon_provider = "nvim-web-devicons",
      views = {
        explorer = {
          width = 0.3,
          height = 0.8,
          kind = "float",
          border = "rounded",
        },
        confirm = {
          width = 0.2,
          height = 0.05,
          kind = "float",
          border = "single",
        },
      },
    } -- check the default options in the README.md
  },
}
