return {
  {
    "nvzone/floaterm",
    dependencies = "nvzone/volt",
    opts = {
      border = true,
      terminals = {
        -- { name = "Terminal", cmd = "btop" },
        { name = "Terminal" },
      }
    },
    keys = {
      { "<C-;>", "<cmd>FloatermToggle<cr>", desc = "Floaterm Toggle", mode = { "n", "x", "t" } },
    },
    cmd = "FloatermToggle",
  }
}
