(import-macros {: map!} :macros)
(vim.pack.add ["https://github.com/folke/snacks.nvim"])

(local Snacks (require "snacks"))

;; setup
(Snacks.setup {
  :words {}
  :git {}
  :gitbrowse {}
  :dim {}
  :notifier {}
  :image {}
  :scope {}

  :statuscolumn
  {:left ["mark" "sign"] ; priority of signs on the left (high to low)
   :right ["fold" "git"] ; priority of signs on the right (high to low)
   :folds {:open true ; show open fold icons
           :git_hl true} ; use Git Signs hl for fold icons
   :git {:patterns ["GitSign" "MiniDiffSign"]}
   :refresh 50} ; refresh at most every 50ms

  :picker
  {:layout {:preset "ivy"}
   :matcher {:fuzzy true
             :frecency true}
   ; :ui_select true
   :debug {:scores true}

   :layouts
   {:telescope
    {:reverse false
     :layout
     {:box "horizontal"
      :backdrop false
      :width 0.8
      :height 0.9
      :border "none"
      {:box "vertical"
       {:win "list" :title " Results " :title_pos "center" :border "rounded"}
       {:win "input" :height 1 :border "rounded" :title "{title} {live} {flags}" :title_pos "center"}}
      {:win "preview"
       :title "{preview:Preview}"
       :width 0.45
       :border "rounded"
       :title_pos "center"}}}

    :default
    {:layout
     {:backdrop false
      :row 1
      :width 0.8
      :min_width 80
      :height 0.8
      :border "rounded"
      :box "horizontal"
      {:box "vertical"
       :border "rounded"
       :title "{title} {live} {flags}"
       :title_pos "center"
       {:win "list" :border "bottom"}
       {:win "input" :height 1 :border "top"}}
      {:win "preview" :title "{preview}" :width 0.4 :border "rounded"}}}

    :ivy
    {:layout
     {:box "vertical"
      :backdrop false
      :row -1
      :width 0
      :height 0.7
      :border "top"
      :title " {title} {live} {flags}"
      :title_pos "left"
      {:win "input" :height 1 :border "bottom"}
      {:box "horizontal"
       {:win "list" :border "none"}
       {:win "preview" :title "{preview}" :width 0.5 :border "none"}}}}

    :vscode
    {:preview "main"
     :layout
     {:backdrop false
      :row 1
      :width 0.4
      :min_width 80
      :height 0.4
      :border "none"
      {:win "input" :height 1 :border "rounded" :title "{title} {live} {flags}" :title_pos "center"}
      {:box "vertical"
        {:win "list" :border "hpad"}
        {:win "preview" :title "{preview}" :height 0.9 :border "rounded"}}}}

   :sources
   {:explorer
    {:finder "explorer"
     :sort {:fields ["sort"]}
     :tree true
     :supports_live true
     :follow_file true
     :auto_close true
     :jump {:close false}
     ; :layout {:preset "ivy" :preview false}
     }}}}})

(map!
  :mode "n"
  :key "+"
  :command (fn []
    (Snacks.picker.explorer))
  :options {:desc "Open Snacks explorer (Directory of Current File)"})

(map!
  :mode "n"
  :key "gO"
  :command (fn []
    (Snacks.picker.lsp_symbols))
  :options {:desc "Open Snacks lsp symbols"})

(map!
  :mode "n"
  :key "gS"
  :command (fn []
    (Snacks.picker.lsp_workspace_symbols))
  :options {:desc "Open Snacks lsp workspace symbols"})

(map!
  :mode "n"
  :key "<leader>sP"
  :command (fn []
    (Snacks.picker))
  :options {:desc "Open Snack picker"})

(map!
  :mode "n"
  :key "<leader>,"
  :command (fn [] (Snacks.picker.buffers))
  :options {:desc "Buffers"})

(map!
  :mode "n"
  :key "<leader>/"
  :command (fn [] (Snacks.picker.files))
  :options {:desc "Grep (Root Dir)"})

(map!
  :mode "n"
  :key "<leader>:"
  :command (fn [] (Snacks.picker.command_history))
  :options {:desc "Command History"})

(map!
  :mode "n"
  :key "<leader><space>"
  :command (fn [] (Snacks.picker.files))
  :options {:desc "Find Files (Root Dir)"})

(map!
  :mode "n"
  :key "<leader>fn"
  :command (fn [] (Snacks.picker.notifications))
  :options {:desc "Notification History"})

;; find
(map!
  :mode "n"
  :key "<leader>fb"
  :command (fn [] (Snacks.picker.buffers))
  :options {:desc "Buffers"})

(map!
  :mode "n"
  :key "<leader>fB"
  :command (fn [] (Snacks.picker.buffers {:hidden true :nofile true}))
  :options {:desc "Buffers (all)"})

(map!
  :mode "n"
  :key "<leader>ff"
  :command (fn [] (Snacks.picker.files))
  :options {:desc "Find Files (Root Dir)"})

(map!
  :mode "n"
  :key "<leader>fF"
  :command (fn [] (Snacks.picker.files {:root true}))
  :options {:desc "Find Files (cwd)"})

(map!
  :mode "n"
  :key "<leader>fg"
  :command (fn [] (Snacks.picker.git_files))
  :options {:desc "Find Files (git-files)"})

(map!
  :mode "n"
  :key "<leader>fr"
  :command (fn [] (Snacks.picker.recent))
  :options {:desc "Recent"})

(map!
  :mode "n"
  :key "<leader>fR"
  :command (fn [] (Snacks.picker.recent {:filter {:cwd true}}))
  :options {:desc "Recent (cwd)"})

(map!
  :mode "n"
  :key "<leader>fp"
  :command (fn [] (Snacks.picker.projects))
  :options {:desc "Projects"})

;; git
(map!
  :mode "n"
  :key "<leader>gd"
  :command (fn [] (Snacks.picker.git_diff))
  :options {:desc "Git Diff (hunks)"})

(map!
  :mode "n"
  :key "<leader>gs"
  :command (fn [] (Snacks.picker.git_status))
  :options {:desc "Git Status"})

(map!
  :mode "n"
  :key "<leader>gS"
  :command (fn [] (Snacks.picker.git_stash))
  :options {:desc "Git Stash"})

;; Grep
(map!
  :mode "n"
  :key "<leader>sb"
  :command (fn [] (Snacks.picker.lines))
  :options {:desc "Buffer Lines"})

(map!
  :mode "n"
  :key "<leader>sB"
  :command (fn [] (Snacks.picker.grep_buffers))
  :options {:desc "Grep Open Buffers"})

(map!
  :mode "n"
  :key "<leader>sg"
  :command (fn [] (Snacks.picker.grep))
  :options {:desc "Grep (Root Dir)"})

(map!
  :mode "n"
  :key "<leader>sP"
  :command (fn [] (Snacks.picker.lazy))
  :options {:desc "Search for Plugin Spec"})

(map!
  :mode ["n" "x"]
  :key "<leader>sw"
  :command (fn [] (Snacks.picker.grep_word))
  :options {:desc "Visual selection or word (Root Dir)"})

;; search
(map!
  :mode "n"
  :key "<leader>s\""
  :command (fn [] (Snacks.picker.registers))
  :options {:desc "Registers"})

(map!
  :mode "n"
  :key "<leader>s/"
  :command (fn [] (Snacks.picker.search_history))
  :options {:desc "Search History"})

(map!
  :mode "n"
  :key "<leader>sa"
  :command (fn [] (Snacks.picker.autocmds))
  :options {:desc "Autocmds"})

(map!
  :mode "n"
  :key "<leader>sc"
  :command (fn [] (Snacks.picker.command_history))
  :options {:desc "Command History"})

(map!
  :mode "n"
  :key "<leader>sC"
  :command (fn [] (Snacks.picker.commands))
  :options {:desc "Commands"})

(map!
  :mode "n"
  :key "<leader>sd"
  :command (fn [] (Snacks.picker.diagnostics))
  :options {:desc "Diagnostics"})

(map!
  :mode "n"
  :key "<leader>sD"
  :command (fn [] (Snacks.picker.diagnostics_buffer))
  :options {:desc "Buffer Diagnostics"})

(map!
  :mode "n"
  :key "<leader>sh"
  :command (fn [] (Snacks.picker.help))
  :options {:desc "Help Pages"})

(map!
  :mode "n"
  :key "<leader>sH"
  :command (fn [] (Snacks.picker.highlights))
  :options {:desc "Highlights"})

(map!
  :mode "n"
  :key "<leader>si"
  :command (fn [] (Snacks.picker.icons))
  :options {:desc "Icons"})

(map!
  :mode "n"
  :key "<leader>sj"
  :command (fn [] (Snacks.picker.jumps))
  :options {:desc "Jumps"})

(map!
  :mode "n"
  :key "<leader>sk"
  :command (fn [] (Snacks.picker.keymaps))
  :options {:desc "Keymaps"})

(map!
  :mode "n"
  :key "<leader>sl"
  :command (fn [] (Snacks.picker.loclist))
  :options {:desc "Location List"})

(map!
  :mode "n"
  :key "<leader>sM"
  :command (fn [] (Snacks.picker.man))
  :options {:desc "Man Pages"})

(map!
  :mode "n"
  :key "<leader>sm"
  :command (fn [] (Snacks.picker.marks))
  :options {:desc "Marks"})

(map!
  :mode "n"
  :key "<leader>sR"
  :command (fn [] (Snacks.picker.resume))
  :options {:desc "Resume"})

(map!
  :mode "n"
  :key "<leader>sq"
  :command (fn [] (Snacks.picker.qflist))
  :options {:desc "Quickfix List"})

(map!
  :mode "n"
  :key "<leader>su"
  :command (fn [] (Snacks.picker.undo))
  :options {:desc "Undotree"})

;; ui
(map!
  :mode "n"
  :key "<leader>uC"
  :command (fn [] (Snacks.picker.colorschemes))
  :options {:desc "Colorschemes"})

(map!
  :mode "n"
  :key "<C-x>4<C-o>"
  :command (fn []
                (vim.cmd.vsplit)
                (Snacks.picker.buffers))
  :options {:desc "Open buffer in split"})

(map!
  :mode "n"
  :key "<leader>un"
  :command (fn [] (Snacks.notifier.hide))
  :options {:desc "Dismiss All Notifications"})
