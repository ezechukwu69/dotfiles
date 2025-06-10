return {
  "Olical/conjure",
  -- [Optional] cmp-conjure for cmp
  dependencies = {
    {
      "PaterJason/cmp-conjure",
    },
  },
  ft = { "clojure", "fennel" }, -- etc
  event = "VeryLazy",
  keys = {
    { "<leader>ee", "<cmd>ConjureEval<cr>",       desc = "Eval line",   mode = "n" },
    { "<leader>eE", "<cmd>ConjureBufferEval<cr>", desc = "Eval buffer", mode = "n" },
    { "<leader>eF", "<cmd>ConjureFileEval<cr>",   desc = "Eval file",   mode = "n" },
  },
  init = function()
    -- Set configuration options here
    vim.g["conjure#debug"] = true
  end,
}
