-- lazy.nvim
return {
  "m4xshen/hardtime.nvim",
  dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
  config = function()
    require("hardtime").setup({
      -- Add "oil" to the disabled_filetypes
      disabled_filetypes = { "qf", "netrw", "NvimTree", "lazy", "mason", "oil" },
    })
  end,
}
