(import-macros {: map!} :macros)

(vim.pack.add [
    "https://github.com/rcarriga/nvim-dap-ui"
    "https://github.com/mfussenegger/nvim-dap"
])

(local dap (require :dap))
(local dapui (require :dapui))
(dapui.setup)

(set dap.listeners.before.attach.dapui_config (fn []
    (dapui.open)
))
(set dap.listeners.before.launch.dapui_config (fn []
    (dapui.open)
))
(set dap.listeners.before.event_terminated.dapui_config (fn []
    (dapui.close)
))
(set dap.listeners.before.event_exited.dapui_config (fn []
    (dapui.close)
))

(local vscode (require "dap.ext.vscode"))
(local json (require "plenary.json"))

(set vscode.json_decode (fn [str]
  (vim.json.decode (json.json_strip_comments str))
))

(require "dap/ruby")

(map!
  :mode "n"
  :key "<leader>dB"
  :command (fn [] (dap.set_breakpoint (vim.fn.input "Breakpoint condition: ")))
  :options {:desc "Breakpoint Condition"})

(map!
  :mode "n"
  :key "<leader>db"
  :command (fn [] (dap.toggle_breakpoint))
  :options {:desc "Toggle Breakpoint"})

(map!
  :mode "n"
  :key "<leader>dc"
  :command (fn [] (dap.continue))
  :options {:desc "Run/Continue"})

;; (map!
;;   :mode "n"
;;   :key "<leader>da"
;;   :command (fn [] (dap.continue {:before get_args}))
;;   :options {:desc "Run with Args"})

(map!
  :mode "n"
  :key "<leader>dC"
  :command (fn [] (dap.run_to_cursor))
  :options {:desc "Run to Cursor"})

(map!
  :mode "n"
  :key "<leader>dg"
  :command (fn [] (dap.goto_))
  :options {:desc "Go to Line (No Execute)"})

(map!
  :mode "n"
  :key "<leader>di"
  :command (fn [] (dap.step_into))
  :options {:desc "Step Into"})

(map!
  :mode "n"
  :key "<leader>dj"
  :command (fn [] (dap.down))
  :options {:desc "Down"})

(map!
  :mode "n"
  :key "<leader>dk"
  :command (fn [] (dap.up))
  :options {:desc "Up"})

(map!
  :mode "n"
  :key "<leader>dl"
  :command (fn [] (dap.run_last))
  :options {:desc "Run Last"})

(map!
  :mode "n"
  :key "<leader>do"
  :command (fn [] (dap.step_out))
  :options {:desc "Step Out"})

(map!
  :mode "n"
  :key "<leader>dO"
  :command (fn [] (dap.step_over))
  :options {:desc "Step Over"})

(map!
  :mode "n"
  :key "<leader>dP"
  :command (fn [] (dap.pause))
  :options {:desc "Pause"})

(map!
  :mode "n"
  :key "<leader>dr"
  :command (fn [] (dap.repl.toggle))
  :options {:desc "Toggle REPL"})

(map!
  :mode "n"
  :key "<leader>du"
  :command (fn [] (dapui.toggle))
  :options {:desc "Toggle REPL"}) ;; Note: This desc is the same as the previous one, might be a copy-paste error.

(map!
  :mode "n"
  :key "<leader>ds"
  :command (fn [] (dap.session))
  :options {:desc "Session"})

(map!
  :mode "n"
  :key "<leader>dt"
  :command (fn [] (dap.terminate))
  :options {:desc "Terminate"})

(map!
  :mode "n"
  :key "<leader>dw"
  :command (fn [] (dap.ui.widgets.hover))
  :options {:desc "Widgets"})
