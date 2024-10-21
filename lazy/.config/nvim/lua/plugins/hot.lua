return {

  {
    "sachinsenal0x64/hot.nvim",
    config = function()
      local opts = require("hot.params").opts

      -- Update the Lualine Status
      Reloader = opts.tweaks.default
      Reloader = "💤"

      Pattern = opts.tweaks.patterns
      Pattern = { "main.py", "main.go" }

      opts.tweaks.start = "🚀"
      opts.tweaks.stop = "💤"
      opts.tweaks.test = "🧪"
      opts.tweaks.test_done = "🧪.✅"
      opts.tweaks.test_fail = "🧪.❌"

      -- If the 'main.*' file doesn't exist, it will fall back to 'index.*'
      opts.tweaks.custom_file = "index"

      -- Add Languages
      opts.set.languages.python = {
        cmd = "python3",
        desc = "Run Python file asynchronously",
        kill_desc = "Kill the running Python file",
        emoji = "🐍",
        test = "python -m unittest -v",
        ext = { ".py" },
      }

      opts.set.languages.go = {
        cmd = "go run",
        desc = "Run Go file asynchronously",
        kill_desc = "Kill the running Go file",
        emoji = "🐹",
        test = "go test",
        ext = { ".go" },
      }

      -- Thot Health Check
      vim.api.nvim_set_keymap(
        "n",
        "<leader>ho",
        '<Cmd>lua require("hot").check()<CR>',
        { noremap = true, silent = true }
      )

      -- Keybinds

      -- Start
      vim.api.nvim_set_keymap(
        "n",
        "<leader>hr",
        '<Cmd>lua require("hot").restart()<CR>',
        { noremap = true, silent = true }
      )
      -- Silent
      vim.api.nvim_set_keymap(
        "n",
        "<leader>hs",
        '<Cmd>lua require("hot").silent()<CR>',
        { noremap = true, silent = true }
      )
      -- Stop
      vim.api.nvim_set_keymap(
        "n",
        "<leader>hx",
        '<Cmd>lua require("hot").stop()<CR>',
        { noremap = true, silent = true }
      )
      -- Test
      vim.api.nvim_set_keymap(
        "n",
        "<leader>ht",
        '<Cmd>lua require("hot").test_restart()<CR>',
        { noremap = true, silent = true }
      )
      -- Close Buffer
      vim.api.nvim_set_keymap(
        "n",
        "<leader>hz",
        '<Cmd>lua require("hot").close_output_buffer()<CR>',
        { noremap = true, silent = true }
      )
      -- Open Buffer
      vim.api.nvim_set_keymap(
        "n",
        "<leader>ha",
        '<Cmd>lua require("hot").open_output_buffer()<CR>',
        { noremap = true, silent = true }
      )

      -- Auto Reload on Save

      local save_group = vim.api.nvim_create_augroup("save_mapping", { clear = true })
      vim.api.nvim_create_autocmd("BufWritePost", {
        desc = "Reloader",
        group = save_group,
        pattern = Pattern,
        callback = function()
          require("hot").silent()
        end,
      })
    end,
  },
}
