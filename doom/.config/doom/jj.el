;;; ../../dotfiles/doom/.config/doom/jj.el -*- lexical-binding: t; -*-

(defun eze/my-presorted-completion-table (completions)
  "Create a completion table that preserves the order of COMPLETIONS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity))
      (complete-with-action action completions string pred))))


(defun eze/jj--get-logs ()
  "Get list of all logs from jj."
  (setq jj-logs-format "jj --no-pager --color never --no-graph -T \"change_id.short()  ++ '	 (' ++ committer.name() ++ ') 	 ' ++ description.first_line() ++ bookmarks.map(|item| ' 	*' ++ item.name() ++ if(item.remote(), '@' ++ item.remote())).join(' ') ++ ' \n'\"")
  (split-string (completing-read
                 "Select a log: "
                 (eze/my-presorted-completion-table
                  (split-string
                   (shell-command-to-string jj-logs-format) "\n" t)                                                            )
                 ) "	")
  )

(defun eze/jj--get-branches()
  "Get list of all branches from jj."
  (setq jj-branch-format "jj b l --color never | awk -F: '/^(^\\w[^:]*:)/ {print $1}'")
  (completing-read
   "branch: "
   (eze/my-presorted-completion-table
    (split-string
     (shell-command-to-string jj-branch-format) "\n" t)                                                            )
   nil
   t)
  )

(defun eze/jj--get-remotes()
  "Get list of all remotes from jj."
  (setq jj-format "jj git remote list | awk '{print $1}'")
  (completing-read
   "remote: "
   (eze/my-presorted-completion-table
    (split-string
     (shell-command-to-string jj-format) "\n" t)))
  )

(defvar my-terminal-color-mode-map (make-sparse-keymap)
  "Keymap for `my-terminal-color-mode'.")

(define-minor-mode my-terminal-color-mode
  "Minor mode to remap `q` to `quit-window` in Evil normal state."
  :init-value nil
  :lighter " TC"
  :keymap my-terminal-color-mode-map)

(defun my-terminal-color-mode-setup ()
  "Set up `my-terminal-color-mode` keybindings."
  (when (bound-and-true-p evil-local-mode)
    (evil-local-set-key 'normal (kbd "q") #'quit-window)))


(add-hook 'my-terminal-color-mode-hook #'my-terminal-color-mode-setup)

(defun eze/show-terminal-colors (buffer)
  (with-current-buffer buffer
    (let* (
           (ansi-text (buffer-string))
           )
      (read-only-mode -1)
      (erase-buffer)
      (insert ansi-text)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1)
      (display-buffer (current-buffer) '(

                                         ))
      (use-local-map (or (current-local-map) (make-sparse-keymap)))

      ;; Create a custom minor mode with high-precedence map
      (my-terminal-color-mode 1)
      (switch-to-buffer-other-frame (current-buffer))
      ))
  )

(defun eze/jj-view ()
  "View logs from jj"
  (interactive
   (list (eze/jj--get-logs))))

(defun eze/jj-focus-buffer ()
  (interactive)
  (eze/show-terminal-colors "*Shell Command Output*")
  (message "Status displayed in *Shell Command Output* buffer.")
  )

(defun eze/jj-push-bookmark (branch remote)
  "Push bookmark to remote"
  (interactive
   (list
    (eze/jj--get-branches)
    (eze/jj--get-remotes)))
  (let (
        (command (format "jj git push -b %s --remote %s" branch remote))
        )
    (shell-command command))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-push-new-bookmark (branch remote)
  "Push bookmark allow new"
  (interactive
   (list
    (eze/jj--get-branches)
    (eze/jj--get-remotes)))
  (let (
        (command (format "jj git push -b %s --remote %s -N" branch remote))
        )
    (shell-command command))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-push-all-tracked-bookmark (remote)
  "Push all tracked bookmarks"
  (interactive
   (list
    (eze/jj--get-remotes)))
  (let (
        (command (format "jj git push --tracked --remote %s" remote))
        )
    (shell-command command))
  (eze/jj-focus-buffer)
  )


(defun eze/jj-set-bookmark (log branch)
  "View logs from jj"
  (interactive
   (list
    (eze/jj--get-logs)
    (eze/jj--get-branches)))
  (let (
        (command (format "jj b s %s -r %s" branch (nth 0 log)))
        )
    (shell-command command))
  (eze/jj-focus-buffer)
  )


(defun eze/jj-status ()
  "View status of the current repository."
  (interactive)
  (shell-command (format "jj st"))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-describe (log message)
  "Describe a revset"
  (interactive
   (list (eze/jj--get-logs)
         (read-string "Enter a message: ")))

  (shell-command (format "jj desc -m \"%s\" %s" message (nth 0 log)))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-remote-add (name url)
  "Add a remote repository with NAME and URL."
  (interactive
   (list
    (read-string "Enter remote name: ")
    (read-string "Enter remote url: ")))

  (shell-command (format "jj git remote add %s %s" name url))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-remote-remove (name)
  "Remove a remote repository with NAME."
  (interactive
   (list
    (eze/jj--get-remotes)))

  (shell-command (format "jj git remote remove %s" name))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-remote-rename(name new_name)
  "Rename a remote repository from NAME to NEW_NAME."
  (interactive
   (list
    (eze/jj--get-remotes)
    (read-string "Enter new remote name: ")
    ))

  (shell-command (format "jj git remote rename %s %s" name new_name))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-remote-set-url(name url)
  "Rename a remote repository from NAME to NEW_NAME."
  (interactive
   (list
    (eze/jj--get-remotes)
    (read-string "Enter new remote name: ")
    ))

  (shell-command (format "jj git remote set-url %s %s" name url))
  (eze/jj-focus-buffer)
  )

(defun eze/jj-remote-list (remote)
  "List all remotes"
  (interactive
   (list
    (eze/jj--get-remotes)))
  (message "%s" remote)
  )

(map!
 :leader
 :prefix "j"
 :n "l" #'eze/jj-view
 :n "d" #'eze/jj-describe
 :n "f" #'eze/jj-focus-buffer
 :n "s" #'eze/jj-status
 :n "b s" #'eze/jj-set-bookmark
 :n "b p" #'eze/jj-push-bookmark
 :n "b P" #'eze/jj-push-new-bookmark
 :n "b a" #'eze/jj-push-all-tracked-bookmark
 :n "r a" #'eze/jj-remote-add
 :n "r r" #'eze/jj-remote-remove
 :n "r n" #'eze/jj-remote-rename
 :n "r u" #'eze/jj-remote-set-url
 :n "r l" #'eze/jj-remote-list
 )
