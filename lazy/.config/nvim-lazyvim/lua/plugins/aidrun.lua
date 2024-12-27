return {
  dir = "~/development/lua/aidrun.nvim",
  config = function()
    local keymap = vim.keymap.set
    keymap("n", "<space>uas", ":Aidrun send<CR>", { desc = "Aidrun send" })
    keymap("n", "<space>uat", ":Aidrun toggle<CR>", { desc = "Aidrun toggle" })
    keymap({ "n", "v" }, "<space>uar", ":Aidrun rewrite<CR>", { desc = "Aidrun rewrite" })
    keymap({ "n", "v" }, "<space>uaa", ":Aidrun ask<CR>", { desc = "Aidrun ask" })
    keymap({ "n", "v" }, "<space>uax", ":Aidrun send_selection<CR>", { desc = "Aidrun send_selection" })
    keymap("n", "<space>uaf", ":Aidrun file_picker<CR>", { desc = "Aidrun file_picker" })
    keymap("n", "<space>uai", ":Aidrun inline<CR>", { desc = "Aidrun inline" })
    require("aidrun").setup({
      options = {
        model = "gemini/gemini-2.0-flash-exp",
        editor_model = "gemini/gemini-2.0-flash-exp",
        show_model_warnings = false,
        dark_mode = true,
        watch = true,
        edit_format = "diff-fenced",
      },
    })
  end,
}
