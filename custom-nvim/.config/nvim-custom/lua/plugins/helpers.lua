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
}
