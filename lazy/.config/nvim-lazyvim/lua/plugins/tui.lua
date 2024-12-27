return {
  'ezechukwu69/tui.nvim',
  config = function()
    require('tui').setup {
      name = 'LazyGitTUI',
      command = 'lazygit',
      height_margin = 2,
      width_margin = 5,
    }

    require('tui').setup {
      name = 'LazyYazi',
      command = 'yazi',
      height_margin = 2,
      width_margin = 5,
    }
  end,
  keys = {
    { '<leader>tg', '<cmd>LazyGitTUI<cr>', desc = '[T]oggle LazyGit TUI' },
    { '<leader>ty', '<cmd>LazyYazi<cr>', desc = '[T]oggle Yazi TUI' },
  },
}
