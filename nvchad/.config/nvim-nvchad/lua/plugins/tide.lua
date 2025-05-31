return {
  {
    "jackMort/tide.nvim",
    event = "BufEnter",
    opts = {
      keys = {
        leader = "|", -- Leader key to prefix all Tide commands
        panel = "|", -- Open the panel (uses leader key as prefix)
        add_item = "a", -- Add a new item to the list (leader + 'a')
        delete = "d", -- Remove an item from the list (leader + 'd')
        clear_all = "x", -- Clear all items (leader + 'x')
        horizontal = "-", -- Split window horizontally (leader + '-')
        vertical = "v", -- Split window vertically (leader + '|')
      }, -- optional configuration
    },
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
  },
}
