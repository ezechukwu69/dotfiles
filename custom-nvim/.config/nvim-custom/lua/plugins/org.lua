return {
  {
    "chipsenkbeil/org-roam.nvim",
    version = "0.1.1",
    ft = "org",
    config = function()
      require("org-roam").setup {
        directory = "~/org/roam"
      }
    end
  },
  {
    "michaelb/sniprun",
    ft = "org",
    build = "sh install.sh",
    config = function()
      require("sniprun").setup()
    end
  },
  {
    "nvim-orgmode/orgmode",
    version = "0.6.0",
    dependencies = {
      {
        "danilshvalov/org-modern.nvim",
      },
      -- {
      --     "nvim-orgmode/telescope-orgmode.nvim",
      --     config = function()
      --         require("telescope").load_extension("orgmode")
      --     end
      -- },
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
    end,
  },
  {
    "lukas-reineke/headlines.nvim",
    ft = "org",
    config = function()
      require("headlines").setup()
    end,
  },
  {
    "massix/org-checkbox.nvim",
    ft = "org",
    config = function()
      require("orgcheckbox").setup()
    end,
  },
  {
    "akinsho/org-bullets.nvim",
    ft = "org",
    config = function()
      require("org-bullets").setup()
    end,
  },
}
