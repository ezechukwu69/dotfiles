return {
  "ezechukwu69/aidrun.nvim",
  -- dir = "~/development/lua/aidrun.nvim",
  config = function()
    local keymap = vim.keymap.set
    keymap("n", "<space>zs", ":Aidrun send<CR>", { desc = "Aidrun send" })
    keymap("n", "<space>zt", ":Aidrun toggle<CR>", { desc = "Aidrun toggle" })
    keymap({ "n", "v" }, "<space>zr", ":Aidrun rewrite<CR>", { desc = "Aidrun rewrite" })
    keymap({ "n", "v" }, "<space>za", ":Aidrun ask<CR>", { desc = "Aidrun ask" })
    keymap({ "n", "v" }, "<space>zx", ":Aidrun send_selection<CR>", { desc = "Aidrun send selection" })
    keymap("n", "<space>zf", ":Aidrun add_file<CR>", { desc = "Aidrun add file" })
    keymap("n", "<space>zF", ":Aidrun file_picker<CR>", { desc = "Aidrun file picker" })
    keymap("n", "<space>zi", ":Aidrun inline<CR>", { desc = "Aidrun inline" })
    keymap("n", "<space>zd", ":Aidrun file_diagnostics<CR>", { desc = "Aidrun file diagnostics" })
    keymap("n", "<space>zD", ":Aidrun workspace_diagnostics<CR>", { desc = "Aidrun workspace diagnostics" })
    keymap("n", "<space>zw", ":Aidrun web<CR>", { desc = "Aidrun web" })
    keymap("n", "<space>zc", ":Aidrun commit<CR>", { desc = "Aidrun commit" })
    keymap("n", "<space>zX", ":Aidrun clear<CR>", { desc = "Aidrun clear" })
    keymap("n", "<space>zp", ":Aidrun paste<CR>", { desc = "Aidrun paste" })
    keymap("n", "<space>ze", ":Aidrun editor<CR>", { desc = "Aidrun editor" })

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
