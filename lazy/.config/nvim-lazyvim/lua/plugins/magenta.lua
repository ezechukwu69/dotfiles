return {
  "dlants/magenta.nvim",
  lazy = false, -- you could also bind to <leader>mt
  enabled = false,
  build = "npm install --frozen-lockfile",
  opts = {
    provider = "anthropic",
    openai = {
      model = "gpt-4o",
    },
    sidebar_position = "right",
    anthropic = {
      model = "claude-3-7-sonnet-latest",
    },
  },
}
