return {
  "folke/which-key.nvim",
  opts = function(_, opts)
    return vim.tbl_deep_extend("force", opts, {
      preset = "helix",
    })
  end,
}
