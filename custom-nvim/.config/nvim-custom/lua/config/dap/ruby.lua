local M = {}

function M.setup()
  local dap = require("dap")

  dap.adapters.ruby = {
    type = 'executable',
    command = 'rdbg',
    args = { '--open', '--port', '12345', '--command', '--' },
  }

  dap.configurations.ruby = {
    {
      type = 'ruby',
      name = 'Debug current file',
      request = 'launch',
      program = '${file}',
    },
    {
      type = "ruby",
      name = "Debug Rails server",
      request = "launch",
      program = "bin/rails",
      programArgs = { "server" },
      useBundler = true,
    },
    {
      type = "ruby",
      name = "Attach to Rails (rdbg)",
      request = "attach",
      remote = true,
      host = "127.0.0.1",
      port = 12345,
    }
  }
end

return M
