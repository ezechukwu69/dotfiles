return {
  "ezechukwu69/aidrun.nvim",
  -- dir = "~/development/lua/aidrun.nvim",
  config = function()
    local keymap = vim.keymap.set
    keymap("n", "<space>as", ":Aidrun send<CR>", { desc = "Aidrun send" })
    keymap("n", "<space>at", ":Aidrun toggle<CR>", { desc = "Aidrun toggle" })
    keymap({ "n", "v" }, "<space>ar", ":Aidrun rewrite<CR>", { desc = "Aidrun rewrite" })
    keymap({ "n", "v" }, "<space>aa", ":Aidrun ask<CR>", { desc = "Aidrun ask" })
    keymap({ "n", "v" }, "<space>ax", ":Aidrun send_selection<CR>", { desc = "Aidrun send selection" })
    keymap("n", "<space>af", ":Aidrun add_file<CR>", { desc = "Aidrun add file" })
    keymap("n", "<space>aF", ":Aidrun file_picker<CR>", { desc = "Aidrun file picker" })
    keymap("n", "<space>ai", ":Aidrun inline<CR>", { desc = "Aidrun inline" })
    keymap("n", "<space>ad", ":Aidrun file_diagnostics<CR>", { desc = "Aidrun file diagnostics" })
    keymap("n", "<space>aD", ":Aidrun workspace_diagnostics<CR>", { desc = "Aidrun workspace diagnostics" })
    keymap("n", "<space>aw", ":Aidrun web<CR>", { desc = "Aidrun web" })
    keymap("n", "<space>ac", ":Aidrun commit<CR>", { desc = "Aidrun commit" })
    keymap("n", "<space>aX", ":Aidrun clear<CR>", { desc = "Aidrun clear" })
    keymap("n", "<space>ap", ":Aidrun paste<CR>", { desc = "Aidrun paste" })
    keymap("n", "<space>ae", ":Aidrun editor<CR>", { desc = "Aidrun editor" })

    require("aidrun").setup({
      options = {
        model = "gemini/gemini-2.0-flash-exp",
        vim = true,
        auto_lint = false,
        browser = false,
        editor_model = "gemini/gemini-2.0-flash-exp",
        show_model_warnings = false,
        dark_mode = true,
        watch = true,
        edit_format = "diff-fenced",
      },
    })
  end,
}
