;;; $DOOMDIR/gptel.el -*- lexical-binding: t; -*-

; (defun my/ls-client-active-p (&optional buffer)
;   "Check if any language server client (lsp-mode or eglot) is active in BUFFER.
; Returns lsp-mode, eglot, or nil."
;   (let ((buf (or buffer (current-buffer))))
;     (with-current-buffer buf
;       (cond
;        ;; Check for lsp-mode
;        ((and (boundp 'lsp-mode) lsp-mode (boundp 'lsp-mode--active-p) (lsp-mode--active-p))
;         'lsp-mode)
;        ;; Check for eglot
;        ((and (boundp 'eglot-mode) eglot-mode (boundp 'eglot--client) (eglot--client))
;         'eglot)
;        (t nil)))))
;
; ;; --- Helper to format LSP Location objects consistently ---
; (defun my/format-lsp-location (loc)
;   "Format an `lsp-location` object (used by both lsp-mode and eglot) consistently.
; Returns a string like 'filename (Range: L:C - L:C)'."
;   (format "%s (Range: %d:%d - %d:%d)"
;           (file-name-nondirectory (lsp-location-uri loc)) ; Filename only
;           (lsp-location-line loc)
;           (lsp-location-character loc)
;           (lsp-location-end-line loc)
;           (lsp-location-end-character loc)))
;
; ;; --- Helper to format LSP Document Symbol objects consistently ---
; (defun my/format-lsp-document-symbol (sym client-type)
;   "Format an LSP document symbol object consistently based on client type.
; Returns a string like 'Name (Kind: Type, Range: L:C - L:C)'."
;   (cond
;    ((eq client-type 'lsp-mode)
;     (format "%s (Kind: %s, Range: %d:%d - %d:%d)"
;             (lsp-symbol-info-name sym)
;             (lsp-symbol-kind-to-string (lsp-symbol-info-kind sym))
;             (line-number-at-pos (lsp-symbol-info-start sym))
;             (save-excursion (goto-char (lsp-symbol-info-start sym)) (current-column))
;             (line-number-at-pos (lsp-symbol-info-end sym))
;             (save-excursion (goto-char (lsp-symbol-info-end sym)) (current-column))))
;    ((eq client-type 'eglot)
;     ;; Eglot returns a plist for symbols
;     (let* ((name (plist-get sym :name))
;            (kind (plist-get sym :kind))
;            (range-start-line (eglot-x--range-start-line (plist-get sym :range)))
;            (range-start-char (eglot-x--range-start-character (plist-get sym :range)))
;            (range-end-line (eglot-x--range-end-line (plist-get sym :range)))
;            (range-end-char (eglot-x--range-end-character (plist-get sym :range))))
;       (format "%s (Kind: %s, Range: %d:%d - %d:%d)"
;               name
;               (lsp-symbol-kind-to-string kind) ; lsp-mode's helper works on numeric kinds
;               range-start-line
;               range-start-char
;               range-end-line
;               range-end-char)))
;    (t "Unknown symbol format")))
;
;
; (defun gptel-goto-line-column (buffer-or-file line column &optional recenter)
;   "Go to LINE and COLUMN in BUFFER-OR-FILE, and optionally recenter the view.
; LINE is 1-indexed, COLUMN is 0-indexed.
; If BUFFER-OR-file is a file path, it will try to find-file-noselect it.
; If RECENTER is non-nil, it will recenter the view."
;   (let* ((buffer (if (stringp buffer-or-file)
;                      (or (get-buffer buffer-or-file)
;                          (find-file-noselect buffer-or-file))
;                    buffer-or-file)))
;     (when buffer
;       (with-current-buffer buffer
;         (goto-line line)
;         (move-to-column column)
;         (when recenter
;           (recenter))))))
;
; ;; --- Helper to format Flycheck diagnostic objects consistently (for Eglot) ---
; (defun my/format-flycheck-diagnostic (diag)
;   "Format a `flycheck-error` object consistently.
; Returns a string like '[LEVEL] Message (File: filename, Range: L:C - L:C)'."
;   (format "[%s] %s (File: %s, Range: %d:%d - %d:%d)"
;           (symbol-name (flycheck-error-level diag)) ; e.g., 'error, 'warning
;           (flycheck-error-message diag)
;           (file-name-nondirectory (flycheck-error-filename diag))
;           (flycheck-error-line diag)     ; Flycheck lines are 1-indexed
;           (flycheck-error-column diag)   ; Flycheck columns are 0-indexed
;           (flycheck-error-end-line diag)
;           (flycheck-error-end-column diag)))
;
; ;; --- System Command Tool ---
; (gptel-make-tool
;  :name "run_command"
;  :function (lambda (command)
;              (shell-command-to-string command))
;  :description "Run a terminal command and return its standard output. Use this for general system operations like 'ls -l', 'grep', 'find', 'git status', etc."
;  :args (list '(:name "command"
;                :type string
;                :description "The terminal command to run."))
;  :category "system")
;
; ;; --- Project/Filesystem Navigation & Listing Tools ---
;
; (gptel-make-tool
;  :name "show_current_directory"
;  :function (lambda ()
;              default-directory)
;  :description "Get the absolute path of the current working directory in Emacs."
;  :args '()
;  :category "fs")
;
; (gptel-make-tool
;  :name "list_files_in_directory"
;  :function (lambda (&optional directory)
;              (let ((default-directory (if directory (expand-file-name directory) default-directory)))
;                (mapconcat (lambda (f)
;                             (format "%s%s" f (if (file-directory-p (expand-file-name f)) "/" "")))
;                           (directory-files default-directory nil nil t)
;                           "\n")))
;  :description "List files and directories in a given directory. Appends '/' to directory names. If no directory is specified, lists the current directory."
;  :args (list '(:name "directory"
;                :type string
;                :description "The path to the directory to list (optional). Defaults to the current directory."
;                :optional t))
;  :category "fs")
;
; (gptel-make-tool
;  :name "show_project_structure"
;  :function (lambda ()
;              (let ((root (or (and (project-current) (project-root (project-current))) default-directory)))
;                (message "Attempting to list project structure from: %s" root)
;                (let ((default-directory root))
;                  (shell-command-to-string "tree -R -a --gitignore -I '/.git|.jj|.elc|__pycache__|node_modules|dist|build|.idea|.vscode/'"))))
;  :description "List the tree structure of the current project recursively. Uses 'tree' command (must be installed), ignoring common VCS and build directories."
;  :args '()
;  :category "project")
;
; ;; --- File CRUD (Create, Read, Update, Delete) Operations ---
;
; (gptel-make-tool
;  :name "create_file"
;  :function (lambda (file_path contents)
;              (let ((expanded_file_path (expand-file-name file_path)))
;                (make-directory (file-name-directory expanded_file_path) t)
;                (with-temp-file expanded_file_path
;                  (insert contents))
;                (format "File '%s' created successfully." expanded_file_path)))
;  :description "Create a new file with the given contents. If the file exists, its contents will be overwritten. Parent directories are created if they don't exist. Path can be absolute or relative."
;  :args (list '(:name "file_path"
;                :type string
;                :description "The path to the file to create (absolute or relative to the current directory).")
;              '(:name "contents"
;                :type string
;                :description "The contents to write to the file."))
;  :category "fs")
;
; (gptel-make-tool
;  :name "read_file"
;  :function (lambda (file_path)
;              (let ((expanded_file_path (expand-file-name file_path)))
;                (if (file-readable-p expanded_file_path)
;                    (with-temp-buffer
;                      (insert-file-contents expanded_file_path)
;                      (buffer-string))
;                  (error "File not found or not readable: %s" expanded_file_path))))
;  :description "Read a file and return its entire contents. Path can be absolute or relative. Returns an error if the file is not found or readable."
;  :args (list
;         '(:name "file_path"
;           :type string
;           :description "The path to the file to read (absolute or relative to the current directory)."))
;  :category "fs")
;
; (gptel-make-tool
;  :name "delete_file"
;  :function (lambda (file_path)
;              (let ((expanded_file_path (expand-file-name file_path)))
;                (if (file-exists-p expanded_file_path)
;                    (progn
;                      (delete-file expanded_file_path)
;                      (format "File '%s' deleted successfully." expanded_file_path))
;                  (error "File not found: %s" expanded_file_path))))
;  :description "Delete a file. Path can be absolute or relative. Returns an error if the file does not exist."
;  :args (list '(:name "file_path"
;                :type string
;                :description "The path to the file to delete (absolute or relative to the current directory)."))
;  :category "fs")
;
; (gptel-make-tool
;  :name "update_file_content"
;  :function (lambda (file_path new_contents)
;              (let ((expanded_file_path (expand-file-name file_path)))
;                (if (file-writable-p expanded_file_path)
;                    (progn
;                      (with-temp-file expanded_file_path
;                        (insert new_contents))
;                      (format "File '%s' updated successfully with new content." expanded_file_path)
;                      ;; If the file is open, jump to its start
;                      (let ((buffer (get-buffer (file-name-nondirectory expanded_file_path))))
;                        (when buffer
;                          (gptel-goto-line-column buffer 1 0 t))))
;                  (error "File not found or not writable: %s" expanded_file_path))))
;  :description "Replace the entire contents of an existing file with new content. If the file does not exist, it will be created. Path can be absolute or relative."
;  :args (list '(:name "file_path"
;                :type string
;                :description "The path to the file to update (absolute or relative to the current directory).")
;              '(:name "new_contents"
;                :type string
;                :description "The new full contents to write to the file."))
;  :category "fs")
;
; (gptel-make-tool
;  :name "append_to_file"
;  :function (lambda (file_path content_to_append)
;              (let ((expanded-file-path (expand-file-name file_path)))
;                (message "DEBUG: append_to_file: Attempting to append to: %s" expanded-file-path)
;
;                (make-directory (file-name-directory expanded-file-path) t)
;
;                (unless (file-writable-p (file-name-directory expanded-file-path))
;                  (error "Directory for '%s' is not writable." file_path))
;
;                (condition-case err
;                    (progn
;                      ;; More efficient implementation using append-to-file
;                      ;; (append-to-file content_to_append nil expanded-file-path)
;
;                      ;; Original implementation (less efficient for large files)
;                      (if (file-exists-p expanded-file-path)
;                          (with-temp-buffer
;                            (insert-file-contents expanded-file-path)
;                            (goto-char (point-max))
;                            (insert content_to_append)
;                            (write-file expanded-file-path))
;                        (with-temp-file expanded-file-path
;                          (insert content_to_append)))
;
;                      (let ((buffer (get-buffer (file-name-nondirectory expanded-file-path))))
;                        (when buffer
;                          (with-current-buffer buffer
;                            (goto-char (point-max))
;                            (recenter))))
;                      (format "Content appended to file '%s'." expanded-file-path))
;                  (error
;                   ;; FIXED: Use `error-message-string` for robust error reporting.
;                   (error "Failed to append to file '%s' due to Emacs Lisp error: %s"
;                          file_path (error-message-string err))))))
;  :description "Append content to the end of an existing file. If the file does not exist, it will be created (and its parent directories). Path can be absolute or relative."
;  :args (list '(:name "file_path"
;                :type string
;                :description "The path to the file to append to (absolute or relative to the current directory).")
;              '(:name "content_to_append"
;                :type string
;                :description "The content string to append to the file."))
;  :category "fs")
;
; ;; --- Buffer Operations ---
;
; (gptel-make-tool
;  :name "list_emacs_buffers"
;  :function (lambda ()
;              (mapconcat (lambda (b) (format "%s (file: %s)" (buffer-name b) (or (buffer-file-name b) "none")))
;                         (buffer-list)
;                         "\n"))
;  :description "List all currently open Emacs buffers, including their names and the file paths they are visiting (if any)."
;  :args '()
;  :category "buffer")
;
; (gptel-make-tool
;  :name "get_buffer_content"
;  :function (lambda (buffer_name)
;              (let ((buffer (get-buffer buffer_name)))
;                (if buffer
;                    (with-current-buffer buffer
;                      (buffer-string))
;                  (error "Buffer not found: %s" buffer_name))))
;  :description "Get the entire content of a specified Emacs buffer. Returns an error if the buffer does not exist."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer (e.g., '*scratch*', 'init.el')."))
;  :category "buffer")
;
; (gptel-make-tool
;  :name "replace_in_buffer"
;  :function (lambda (buffer_name old_text new_text)
;              (let ((buffer (get-buffer buffer_name)))
;                (if buffer
;                    (with-current-buffer buffer
;                      (goto-char (point-min))
;                      (let ((count (replace-string old_text new_text nil (point-min) (point-max))))
;                        (format "Replaced '%s' with '%s' %d times in buffer '%s'."
;                                old_text new_text count buffer_name))
;                      ;; Jump to the start of the buffer to show the effect of replacement
;                      (gptel-goto-line-column (buffer-name) 1 0 t))
;                  (error "Buffer not found: %s" buffer_name))))
;  :description "Replace all occurrences of 'old_text' with 'new_text' within a specified Emacs buffer. Returns the number of replacements made. Note: This modifies the buffer in memory, but does NOT save the file to disk."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to modify.")
;              '(:name "old_text"
;                :type string
;                :description "The text to search for.")
;              '(:name "new_text"
;                :type string
;                :description "The text to replace with."))
;  :category "buffer")
;
; (gptel-make-tool
;  :name "insert_into_buffer"
;  :function (lambda (buffer_name position content)
;              (let ((buffer (get-buffer buffer_name))
;                    (result-message ""))
;                (if buffer
;                    (with-current-buffer buffer
;                      (cond
;                       ((string= position "start") (goto-char (point-min)))
;                       ((string= position "end") (goto-char (point-max)))
;                       ((string-match-p "^[0-9]+$" position)
;                        (let ((char-pos (string-to-number position)))
;                          (if (< char-pos 0)
;                              (goto-char (+ (point-max) char-pos))
;                            (goto-char char-pos))))
;                       (t (error "Invalid position: %s. Use 'start', 'end', or a character index." position)))
;                      (insert content)
;                      (setq result-message (format "Content inserted into buffer '%s' at position '%s'." buffer_name position))
;                      ;; Jump to the location where content was inserted
;                      (gptel-goto-line-column (buffer-name) (line-number-at-point) (current-column) t))
;                  (error "Buffer not found: %s" buffer_name))
;                result-message))
;  :description "Insert content into a specified Emacs buffer at a given position. Position can be 'start', 'end', or a character index (0-indexed). Negative indices count from the end of the buffer. Note: This modifies the buffer in memory, but does NOT save the file to disk."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to modify.")
;              '(:name "position"
;                :type string
;                :description "The position to insert at: 'start', 'end', or a character index (e.g., '100'). Negative index counts from end.")
;              '(:name "content"
;                :type string
;                :description "The content string to insert."))
;  :category "buffer")
;
; (gptel-make-tool
;  :name "save_buffer_to_file"
;  :function (lambda (buffer_name)
;              (let ((buffer (get-buffer buffer_name)))
;                (if buffer
;                    (with-current-buffer buffer
;                      (if (buffer-file-name)
;                          (progn
;                            (save-buffer)
;                            (format "Buffer '%s' saved to file '%s'." buffer_name (buffer-file-name)))
;                        (error "Buffer '%s' is not visiting a file. Use 'write_buffer_to_new_file' instead." buffer_name)))
;                  (error "Buffer not found: %s" buffer_name))))
;  :description "Save the contents of a buffer to its associated file on disk. Returns an error if the buffer is not visiting a file (i.e., it's unsaved or a special buffer like '*scratch*')."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to save."))
;  :category "buffer")
;
; (gptel-make-tool
;  :name "write_buffer_to_new_file"
;  :function (lambda (buffer_name file_path)
;              (let ((buffer (get-buffer buffer_name))
;                    (expanded_file_path (expand-file-name file_path)))
;                (if buffer
;                    (progn
;                      (make-directory (file-name-directory expanded_file_path) t)
;                      (with-current-buffer buffer
;                        (write-file expanded_file_path))
;                      (format "Buffer '%s' written to new file '%s'." buffer_name expanded_file_path)
;                      ;; Jump to the start of the new file (or the buffer if already open)
;                      (gptel-goto-line-column (buffer-name) 1 0 t))) ; Jump to line 1, column 0 of the buffer
;                (error "Buffer not found: %s" buffer_name)))
;  :description "Write the contents of a specified Emacs buffer to a new file on disk. This will create or overwrite the file. Parent directories are created if they don't exist."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to write.")
;              '(:name "file_path"
;                :type string
;                :description "The absolute or relative path to the new file."))
;  :category "buffer")
;
; (gptel-make-tool
;  :name "kill_buffer"
;  :function (lambda (buffer_name)
;              (let ((buffer (get-buffer buffer_name)))
;                (if buffer
;                    (progn
;                      (kill-buffer buffer)
;                      (format "Buffer '%s' killed." buffer_name))
;                  (error "Buffer not found: %s" buffer_name))))
;  :description "Kill (close) a specified Emacs buffer. Unsaved changes will be lost unless the buffer is visiting a file and Emacs prompts to save."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to kill."))
;  :category "buffer")
;
; ;; FIXED: Corrected the syntax error where :description and :args were inside the lambda.
; (gptel-make-tool
;  :name "find_file_and_open"
;  :function (lambda (file_path)
;              (let ((expanded_file_path (expand-file-name file_path)))
;                (if (file-exists-p expanded_file_path)
;                    (progn
;                      (find-file expanded_file_path)
;                      (format "File '%s' opened in buffer '%s'." expanded_file_path (buffer-name (current-buffer)))
;                      ;; Jump to the start of the opened file
;                      (gptel-goto-line-column (buffer-name) 1 0 t))
;                  (error "File not found: %s" expanded_file_path))))
;  :description "Open a file in a new or existing Emacs buffer. If the file is already open, Emacs will switch to its buffer. Returns an error if the file does not exist."
;  :args (list '(:name "file_path"
;                :type string
;                :description "The path to the file to open (absolute or relative to the current directory)."))
;  :category "buffer")
;
;
; ;; --- Advanced Text Replacement (File & Buffer) ---
;
; ;; Internal helper for `replace_text_in_range`.
; (defun my--replace-text-in-range-internal (replacement_string start_line start_column end_line end_column)
;   "Internal helper for `replace_text_in_range`.
;   Assumes the correct buffer/file is already current."
;   (save-excursion
;     (save-restriction
;       (widen)
;
;       (let (start-point end-point)
;         (goto-line start_line)
;         (move-to-column start_column)
;         (setq start-point (point))
;
;         (goto-line end_line)
;         (move-to-column end_column)
;         (setq end-point (point))
;
;         (delete-region start-point end-point)
;         (goto-char start-point)
;         (insert replacement_string)
;
;         (format "Replaced text from %d:%d to %d:%d with '%s'."
;                 start_line start_column end_line end_column
;                 replacement_string))))))
;
; ;; FIXED: Corrected the logical error of using `with-temp-file` to modify an existing file.
; (gptel-make-tool
;  :name "replace_text_in_range"
;  :function (lambda (replacement_string start_line start_column end_line end_column &optional target_path)
;              (let ((result ""))
;                (if target_path
;                    ;; Case 1: A file path is provided.
;                    (let* ((expanded_path (expand-file-name target_path))
;                           ;; Find file without selecting the window, get the buffer.
;                           (buffer (find-file-noselect expanded_path)))
;                      (with-current-buffer buffer
;                        (setq result (my--replace-text-in-range-internal
;                                      replacement_string start_line start_column end_line end_column))
;                        (save-buffer)) ; Persist changes to disk.
;                      ;; Jump to the location in the buffer associated with the file.
;                      (gptel-goto-line-column buffer start_line start_column t))
;                  ;; Case 2: No file path, operate on the current buffer.
;                  (progn
;                    (setq result (my--replace-text-in-range-internal
;                                  replacement_string start_line start_column end_line end_column))
;                    ;; Jump to the location in the current buffer.
;                    (gptel-goto-line-column (current-buffer) start_line start_column t)))
;                (message result)
;                result))
;  :description "Replace text within a specified line/column range with REPLACEMENT_STRING.
;   Line numbers are 1-indexed, column numbers are 0-indexed.
;   Can operate on the current active Emacs buffer or a specified file path.
;   Note: When operating on a file, changes are saved directly to disk.
;   When operating on a buffer, changes are made in memory but the buffer must be saved to persist them to a file."
;  :args (list '(:name "replacement_string" :type string :description "The string to insert as the replacement.")
;              '(:name "start_line" :type number :description "The starting line number (1-indexed).")
;              '(:name "start_column" :type number :description "The starting column number (0-indexed).")
;              '(:name "end_line" :type number :description "The ending line number (1-indexed).")
;              '(:name "end_column" :type number :description "The ending column number (0-indexed).")
;              '(:name "target_path"
;                :type string
;                :description "Optional: The path to the file to modify. If omitted, the operation is performed on the current Emacs buffer."
;                :optional t))
;  :category "text_manipulation")
;
;
; ;; --- LSP Code Symbols Tools (Supports lsp-mode and Eglot) ---
;
; (gptel-make-tool
;  :name "lsp_get_document_symbols"
;  :function (lambda (&optional file_path)
;              (let* ((buffer (if file_path (find-file-noselect (expand-file-name file_path)) (current-buffer)))
;                     (client-type (my/ls-client-active-p buffer))
;                     (symbols-info ""))
;                (if client-type
;                    (with-current-buffer buffer
;                      (cond
;                       ((eq client-type 'lsp-mode)
;                        ;; Ensure lsp-mode is truly active and ready for the buffer
;                        (lsp--force-sync-if-needed)
;                        ;; NOTE: `lsp-bridge-document-symbol-list` is specific to the `lsp-bridge` package.
;                        ;; A standard `lsp-mode` setup might require a different function.
;                        (setq symbols-info (mapconcat (lambda (sym) (my/format-lsp-document-symbol sym 'lsp-mode))
;                                                      (lsp-bridge-document-symbol-list) "\n")))
;                       ((eq client-type 'eglot)
;                        (setq symbols-info (mapconcat (lambda (sym) (my/format-lsp-document-symbol sym 'eglot))
;                                                      (eglot-document-symbols) "\n")))))
;                  (error "No LSP client (lsp-mode or eglot) active for %s. Ensure it's enabled and a language server is running."
;                         (if file_path file_path (buffer-name buffer))))
;                symbols-info))
;  :description "Get a list of all symbols (functions, classes, variables, etc.) defined in the current buffer or a specified file. Each symbol includes its name, kind, and line/column range. Requires an LSP client (lsp-mode or eglot) to be active for the target file/buffer."
;  :args (list '(:name "file_path"
;                :type string
;                :description "Optional: The path to the file to get symbols from. If omitted, uses the current buffer."
;                :optional t))
;  :category "lsp_analysis")
;
; (gptel-make-tool
;  :name "lsp_find_references"
;  :function (lambda (file_path line column)
;              (let* ((expanded_file_path (expand-file-name file_path))
;                     (buffer (find-file-noselect expanded_file_path))
;                     (client-type (my/ls-client-active-p buffer))
;                     (references-info ""))
;                (if client-type
;                    (with-current-buffer buffer
;                      (goto-line line)
;                      (move-to-column column)
;                      (cond
;                       ((eq client-type 'lsp-mode)
;                        (lsp--force-sync-if-needed)
;                        (let ((refs (lsp-find-references)))
;                          (setq references-info (mapconcat #'my/format-lsp-location refs "\n"))))
;                       ((eq client-type 'eglot)
;                        (let ((refs (eglot-find-references)))
;                          (setq references-info (mapconcat #'my/format-lsp-location refs "\n")))))
;                      (message "Found references to symbol at %s:%d:%d" file_path line column))
;                  (error "No LSP client active for %s. Ensure it's enabled and a language server is running." file_path))
;                references-info))
;  :description "Finds all references to the symbol at the specified line and column in a given file. Returns a list of locations where the symbol is referenced. Requires an LSP client (lsp-mode or eglot) to be active for the target file."
;  :args (list '(:name "file_path" :type string :description "The path to the file containing the symbol.")
;              '(:name "line" :type number :description "The 1-indexed line number of the symbol.")
;              '(:name "column" :type number :description "The 0-indexed column number of the symbol."))
;  :category "lsp_analysis")
;
; (gptel-make-tool
;  :name "lsp_find_definition"
;  :function (lambda (file_path line column)
;              (let* ((expanded_file_path (expand-file-name file_path))
;                     (buffer (find-file-noselect expanded_file_path))
;                     (client-type (my/ls-client-active-p buffer))
;                     (definitions-info ""))
;                (if client-type
;                    (with-current-buffer buffer
;                      (goto-line line)
;                      (move-to-column column)
;                      (cond
;                       ((eq client-type 'lsp-mode)
;                        (lsp--force-sync-if-needed)
;                        (let ((defs (lsp-find-definition)))
;                          (setq definitions-info (mapconcat #'my/format-lsp-location defs "\n"))))
;                       ((eq client-type 'eglot)
;                        (let ((defs (eglot-find-definition)))
;                          (setq definitions-info (mapconcat #'my/format-lsp-location defs "\n")))))
;                      (message "Found definition for symbol at %s:%d:%d" file_path line column))
;                  (error "No LSP client active for %s. Ensure it's enabled and a language server is running." file_path))
;                definitions-info))
;  :description "Finds the definition(s) of the symbol at the specified line and column in a given file. Returns a list of locations where the symbol is defined. Requires an LSP client (lsp-mode or eglot) to be active for the target file."
;  :args (list '(:name "file_path" :type string :description "The path to the file containing the symbol.")
;              '(:name "line" :type number :description "The 1-indexed line number of the symbol.")
;              '(:name "column" :type number :description "The 0-indexed column number of the symbol."))
;  :category "lsp_analysis")
;
; (gptel-make-tool
;  :name "lsp_get_diagnostics"
;  :function (lambda (&optional file_path)
;              (let* ((buffer (if file_path (find-file-noselect (expand-file-name file_path)) (current-buffer)))
;                     (client-type (my/ls-client-active-p buffer))
;                     (diagnostics-info ""))
;                (if client-type
;                    (with-current-buffer buffer
;                      (cond
;                       ((eq client-type 'lsp-mode)
;                        (lsp--force-sync-if-needed)
;                        (let ((diagnostics (lsp-diagnostics--buffer-diagnostics buffer)))
;                          (setq diagnostics-info (mapconcat (lambda (diag)
;                                                              (format "[%s] %s (Range: %d:%d - %d:%d)"
;                                                                      (lsp-diag-level-to-string (lsp-diagnostic-severity diag))
;                                                                      (lsp-diagnostic-message diag)
;                                                                      (line-number-at-pos (lsp-diagnostic-range-start diag))
;                                                                      (save-excursion (goto-char (lsp-diagnostic-range-start diag)) (current-column))
;                                                                      (line-number-at-pos (lsp-diagnostic-range-end diag))
;                                                                      (save-excursion (goto-char (lsp-diagnostic-range-end diag)) (current-column))))
;                                                            diagnostics "\n"))))
;                       ((eq client-type 'eglot)
;                        ;; Eglot uses Flycheck for diagnostics
;                        (flycheck-buffer) ; Ensure diagnostics are up-to-date
;                        (let ((diagnostics (flycheck-list-errors)))
;                          (setq diagnostics-info (mapconcat #'my/format-flycheck-diagnostic diagnostics "\n"))))))
;                  (error "No LSP client (lsp-mode or eglot) active for %s. Ensure it's enabled and a language server is running."
;                         (if file_path file_path (buffer-name buffer))))
;                diagnostics-info))
;  :description "Get a list of diagnostic messages (errors, warnings, info) for the current buffer or a specified file. Each diagnostic includes its severity, message, and location. Requires an LSP client (lsp-mode or eglot) to be active for the target file/buffer."
;  :args (list '(:name "file_path"
;                :type string
;                :description "Optional: The path to the file to get diagnostics from. If omitted, uses the current buffer."
;                :optional t))
;  :category "lsp_analysis")
;
; ;; --- Basic Git/VCS Interaction (assuming Git is installed and in PATH) ---
;
; (gptel-make-tool
;  :name "git_status"
;  :function (lambda ()
;              (let ((default-directory (or (and (project-current) (project-root (project-current))) default-directory)))
;                (if (vc-backend default-directory)
;                    (shell-command-to-string "git status")
;                  (error "Not a Git repository at %s" default-directory))))
;  :description "Get the current Git repository status. Automatically finds the project root. Returns an error if not in a Git repository."
;  :args '()
;  :category "vcs")
;
; (gptel-make-tool
;  :name "git_diff"
;  :function (lambda (&optional file_path)
;              (let ((default-directory (or (and (project-current) (project-root (project-current))) default-directory)))
;                (if (vc-backend default-directory)
;                    (if file_path
;                        (shell-command-to-string (format "git diff %s" (shell-quote-argument (expand-file-name file_path))))
;                      (shell-command-to-string "git diff"))
;                  (error "Not a Git repository at %s" default-directory))))
;  :description "Show changes between the working tree and the index/HEAD. Can be limited to a specific file. Automatically finds the project root. Returns an error if not in a Git repository."
;  :args (list '(:name "file_path" :type string :description "Optional: The path to a specific file to diff." :optional t))
;  :category "vcs")
;
; (gptel-make-tool
;  :name "git_add"
;  :function (lambda (file_paths)
;              (let ((default-directory (or (and (project-current) (project-root (project-current))) default-directory)))
;                (if (vc-backend default-directory)
;                    (shell-command-to-string (format "git add %s" (mapconcat #'shell-quote-argument file_paths " ")))
;                  (error "Not a Git repository at %s" default-directory))))
;  :description "Stage changes for specified files. Takes a list of file paths. Automatically finds the project root. Returns an error if not in a Git repository."
;  :args (list '(:name "file_paths"
;                :type (array string)
;                :description "A list of file paths (relative or absolute) to stage."))
;  :category "vcs")
;
; (gptel-make-tool
;  :name "git_commit"
;  :function (lambda (message)
;              (let ((default-directory (or (and (project-current) (project-root (project-current))) default-directory)))
;                (if (vc-backend default-directory)
;                    (shell-command-to-string (format "git commit -m %s" (shell-quote-argument message)))
;                  (error "Not a Git repository at %s" default-directory))))
;  :description "Commit staged changes with a given message. Automatically finds the project root. Returns an error if not in a Git repository."
;  :args (list '(:name "message" :type string :description "The commit message."))
;  :category "vcs")
;
; ;; --- Development Workflow Tools ---
;
; (gptel-make-tool
;  :name "run_project_tests"
;  :function (lambda (&optional test_command)
;              (let* ((root (or (and (project-current) (project-root (project-current))) default-directory))
;                     (command (or test_command "make test" "npm test" "pytest" "cabal test" "mix test" "go test ./...")))
;                (message "Running tests in project root: %s with command: %s" root command)
;                (let ((default-directory root))
;                  (shell-command-to-string command)))
;              :description "Run the tests for the current project. Attempts to find a common test command if not specified (e.g., 'npm test', 'pytest'). Runs from the project root."
;              :args (list '(:name "test_command"
;                            :type string
;                            :description "Optional: The specific command to run tests (e.g., 'pytest tests/my_test.py')."
;                            :optional t)))
;  :category "dev_workflow")
;
; (gptel-make-tool
;  :name "compile_project"
;  :function (lambda (&optional compile_command)
;              (let* ((root (or (and (project-current) (project-root (project-current))) default-directory))
;                     (command (or compile_command "make" "npm build" "mvn clean install" "cabal build" "go build ./...")))
;                (message "Running build command: %s in project root: %s" command root)
;                (let ((default-directory root))
;                  (shell-command-to-string command))))
;  :description "Run the build/compile command for the current project. Attempts to find a common build command if not specified (e.g., 'make', 'npm build'). Runs from the project root."
;  :args (list '(:name "compile_command"
;                :type string
;                :description "Optional: The specific command to run compilation (e.g., 'make', 'npm build')."
;                :optional t))
;  :category "dev_workflow")
;
; ;; --- Emacs Environment Variables ---
;
; (gptel-make-tool
;  :name "get_emacs_variable"
;  :function (lambda (variable_name)
;              (let ((sym (intern-soft variable_name)))
;                (if (boundp sym)
;                    (format "Variable '%s' has value: %S" variable_name (symbol-value sym))
;                  (error "Variable '%s' is not defined." variable_name))))
;  :description "Get the current value of an Emacs Lisp variable (e.g., 'fill-column', 'package-archives')."
;  :args (list '(:name "variable_name" :type string :description "The name of the Emacs Lisp variable."))
;  :category "emacs_env")
;
; (gptel-make-tool
;  :name "set_emacs_variable"
;  :function (lambda (variable_name value)
;              (let ((sym (intern-soft variable_name)))
;                (if (boundp sym)
;                    (progn
;                      (set sym value)
;                      (format "Variable '%s' set to: %S" variable_name (symbol-value sym)))
;                  (error "Variable '%s' is not defined, cannot set." variable_name))))
;  :description "Set the value of an Emacs Lisp variable. Note: This changes the variable's value for the current Emacs session only, not persistently across restarts."
;  :args (list '(:name "variable_name" :type string :description "The name of the Emacs Lisp variable.")
;              '(:name "value" :type any :description "The value to set the variable to. Type should match the variable's expected type (e.g., string, number, boolean)."))
;  :category "emacs_env")
;
;
; (gptel-make-tool
;  :name "insert_and_get_buffer_content"
;  :function (lambda (buffer_name position content)
;              (let ((buffer (get-buffer buffer_name))
;                    (result-message ""))
;                (if buffer
;                    (with-current-buffer buffer
;                      (cond
;                       ((string= position "start") (goto-char (point-min)))
;                       ((string= position "end") (goto-char (point-max)))
;                       ((string-match-p "^[0-9]+$" position)
;                        (let ((char-pos (string-to-number position)))
;                          (if (< char-pos 0)
;                              (goto-char (+ (point-max) char-pos))
;                            (goto-char char-pos))))
;                       (t (error "Invalid position: %s. Use 'start', 'end', or a character index." position)))
;                      (insert content)
;                      (setq result-message (format "Content inserted into buffer '%s' at position '%s'. Buffer content: %s"
;                                                   buffer_name position (buffer-string)))
;                      ;; Jump to the location where content was inserted
;                      (gptel-goto-line-column (buffer-name) (line-number-at-point) (current-column) t))
;                  (error "Buffer not found: %s" buffer_name))
;                result-message))
;  :description "Insert content into a specified Emacs buffer at a given position and then return the entire content of that buffer. Position can be 'start', 'end', or a character index (0-indexed). Negative indices count from the end of the buffer. Note: This modifies the buffer in memory, but does NOT save the file to disk."
;  :args (list '(:name "buffer_name"
;                :type string
;                :description "The name of the Emacs buffer to modify.")
;              '(:name "position"
;                :type string
;                :description "The position to insert at: 'start', 'end', or a character index (e.g., '100'). Negative index counts from end.")
;              '(:name "content"
;                :type string
;                :description "The content string to insert."))
;  :category "buffer")
;
