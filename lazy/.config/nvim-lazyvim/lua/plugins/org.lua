return {
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    config = function()
      require("orgmode").setup({
        org_agenda_files = { "~/org/agenda.org" },
        org_default_notes_file = "~/org/personal.org",
      })
    end,
  }
}
