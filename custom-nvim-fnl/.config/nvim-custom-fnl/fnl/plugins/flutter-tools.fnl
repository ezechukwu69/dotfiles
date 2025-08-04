(import-macros {: map!} :macros)
(vim.pack.add [
    "https://github.com/akinsho/flutter-tools.nvim"
 ])

(local dap (require :dap))

(local flutter (require :flutter-tools))

(flutter.setup {:decorations {:statusline {:device true
                               :app_version true
                               :project_config true}}
   :debugger {:enabled true
              :exception_breakpoints ["raised" "user-unhandled"]}
   :dev_tools {:autostart true
               :auto_open_browser true}
   :outline {:auto_open false}
   :lsp {:color {:enabled true ; whether or not to highlight color variables at all, only supported on flutter >= 2.10
                 :background true ; highlight the background
                 ; :foreground , ; highlight the foreground
                 :virtual_text true ; show the highlight using virtual text
                 :virtual_text_str "■"}}
   :register_configurations (fn [paths]
                              (tset dap.configurations :dart
                                {:name "Flutter: Run"
                                 :type "dart"
                                 :request "launch"}
                                {:name "Flutter: Run (Verbose)"
                                 :type "dart"
                                 :request "launch"
                                 :args ["-v"]})
                              ((. (require "dap.ext.vscode") :load_launchjs)))
   :widget_guides {:enabled true}
   :dev_log {:enabled false
             ; Open as split horizontal
             :open_cmd "split"}})

(local my-ft-group (vim.api.nvim_create_augroup :MyFiletypeSettings {:clear true}))

;; 2. Define the autocommand for 'dart'
(vim.api.nvim_create_autocmd :FileType
  {:group my-ft-group
   :pattern "dart"
   :callback (fn []
      (map!
        :mode "n"
        :key "<localleader>fe"
        :command (fn [] (vim.cmd.FlutterEmulators))
        :options {:desc "Flutter Emulators" })

      (map!
        :mode "n"
        :key "<localleader>fg"
        :command (fn [] (vim.cmd.FlutterPubGet))
        :options {:desc "Flutter Pub Get" })

      (map!
        :mode "n"
        :key "<localleader>fs"
        :command (fn [] (vim.cmd.FlutterRun))
        :options {:desc "Flutter Run" })

      (map!
        :mode "n"
        :key "<localleader>fr"
        :command (fn [] (vim.cmd.FlutterReload))
        :options {:desc "Flutter Reload" })

      (map!
        :mode "n"
        :key "<localleader>fR"
        :command (fn [] (vim.cmd.FlutterRestart))
        :options {:desc "Flutter Restart" })
      )
})

