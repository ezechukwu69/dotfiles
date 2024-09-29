return {
  'olimorris/codecompanion.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-treesitter/nvim-treesitter',
    'hrsh7th/nvim-cmp', -- Optional: For using slash commands and variables in the chat buffer
    {
      'stevearc/dressing.nvim', -- Optional: Improves the default Neovim UI
      opts = {},
    },
    'nvim-telescope/telescope.nvim', -- Optional: For using slash commands
  },
  keys = {
    { '<leader>ac', '<cmd>CodeCompanionChat<CR>', desc = 'Code companion chat' },
    { '<leader>at', '<cmd>CodeCompanionToggle<CR>', desc = 'Code companion toggle' },
    { '<leader>aa', '<cmd>CodeCompanionActions<CR>', desc = 'Code companion toggle' },
  },
  config = true,
}
