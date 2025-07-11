return {
  {
    "nvim-orgmode/orgmode",
    ft = { 'org' },
    tag = "0.6.0",
    dependencies = {
      {
        "chipsenkbeil/org-roam.nvim",
        ft = { 'org' },
        tag = "0.1.1",
      },
      {
        "michaelb/sniprun",
        build = "sh install.sh",
        config = function()
          require("sniprun").setup()
        end
      },
      { "akinsho/org-bullets.nvim" },
      {
        "lukas-reineke/headlines.nvim",
        dependencies = "nvim-treesitter/nvim-treesitter",
      },
      -- {
      --     "nvim-orgmode/telescope-orgmode.nvim",
      --     config = function()
      --         require("telescope").load_extension("orgmode")
      --     end
      -- },
      {
        "danilshvalov/org-modern.nvim"
      },
      {
        "massix/org-checkbox.nvim",
      }
    },
    config = function()
      local Menu = require("org-modern.menu")
      local org = require('orgmode')
      require("orgmode").setup({
        org_agenda_files = { "~/org/agenda.org" },
        org_default_notes_file = "~/org/agenda.org",
        ui = {
          menu = {
            handler = function(data)
              Menu:new({
                window = {
                  margin = { 1, 0, 1, 0 },
                  padding = { 0, 1, 0, 1 },
                  title_pos = "center",
                  border = "single",
                  zindex = 1000,
                },
                icons = {
                  separator = "➜",
                },
              }):open(data)
            end,
          },
        },
      })
      require("org-roam").setup {
        directory = "~/org/roam"
      }
      require("org-bullets").setup()
      require("headlines").setup()
      require("orgcheckbox").setup()
    end,
  }
}
