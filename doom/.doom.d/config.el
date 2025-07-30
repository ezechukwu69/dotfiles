;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(defmacro incf (place &optional delta)
  `(cl-incf ,place ,delta))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-font (font-spec :family "Aporetic Serif Mono" :size 14 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "Aporetic Serif Mono" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-agenda-files (directory-files-recursively org-directory "\.org$")
      org-roam-directory (file-truename "~/org/roam"))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :leader :desc "Navigate to right window" :n "C-l" #'evil-window-right)
(map! :leader :desc "Navigate to left window" :n "C-h" #'evil-window-left)
(map! :leader :desc "Navigate to down window" :n "C-j" #'evil-window-down)
(map! :leader :desc "Navigate to up window" :n "C-k" #'evil-window-up)


(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (dart "https://github.com/UserNobody14/tree-sitter-dart")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(setq treesit-font-lock-level 4)
(add-hook 'dart-mode-hook #'tree-sitter-hl-mode)
(global-tree-sitter-mode)

(use-package! copilot
  :hook '((prog-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("M-l" . #'copilot-accept-completion)
              ("TAB" . #'copilot-accept-completion)
              ("C-TAB" . #'copilot-accept-completion-by-word)
              ("C-<tab>" . #'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode  2))
  (add-to-list 'copilot-indentation-alist '(org-mode  2))
  (add-to-list 'copilot-indentation-alist '(text-mode  2))
  (add-to-list 'copilot-indentation-alist '(closure-mode  2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2)))

(use-package! gptel
  :config
  ;; OPTIONAL configuration

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user
")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant
")
  (setq
   ;; gptel-model 'gemini-2.0-flash
   ;; gptel-model 'gemini-2.5-flash-preview-04-17
   ;; gptel-include-reasoning ''
   gptel-default-mode 'org-mode
   gptel-model 'gemini-2.5-flash-preview-05-20
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (getenv "GEMINI_API_KEY")
                   :stream t)))


(use-package! aider
  :config
  (setq aider-args '("--model"
                     "gemini/gemini-2.5-flash-preview-05-20"
                     "--no-attribute-author"
                     "--no-auto-commits"
                     "--no-dirty-commits"
                     "--architect"
                     "")))


(use-package! flyover
  :hook '((prog-mode . flycheck-mode)
          (flycheck-mode-hook . flyover-mode))
  :config
  (setq flyover-checkers '(flycheck flymake))
  (setq flyover-info-icon "🛈")
  (setq flyover-warning-icon "⚠")
  (setq flyover-error-icon "✘")
  (setq flyover-icon-left-padding 0.9)
  (setq flyover-icon-right-padding 0.9)
  (setq flyover-line-position-offset 1)
  (setq flyover-levels '(error)))

(setq lsp-dart-dap-flutter-hot-reload-on-save t)

(use-package! expand-region
  :bind
  (("M-=" . er/expand-region)
   ("M--" . er/contract-region)))

(use-package! prisma-mode
  :config
  (setq prisma-format-on-save t))

;; (use-package! mcp
;;   :after gptel
;;   :custom (mcp-hub-servers
;;            `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/lizqwer/MyProject/")))
;;              ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))

;; :config
;; (require 'mcp-hub)
;; (require 'gptel-integrations)
;; :hook
;; (after-init . mcp-hub-start-all-server))


(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(setq which-key-use-C-h-commands t
      which-key-paging-prefixes '("C-x")
      which-key-paging-key "<f5>"
      evil-snipe-scope 'buffer
      avy-all-windows t)

;; (map! :leader
;;       :desc "Show project diagnostics"
;;       :n "c X" #'flymake-show-project-diagnostics)


;; (add-hook 'dart-mode-hook
;;           (lambda ()
;;             (setq lsp-dart-dap-flutter-hot-reload-on-save t)))
;;

;; (add-hook 'after-save-hook (lambda ()
;;                              (when (eq major-mode 'dart-mode)
;;                                (flutter-run-or-hot-reload))))



(add-hook 'prog-mode-hook #'completion-preview-mode)
(setq lsp-dart-dap-flutter-hot-reload-on-save t)

(setq corfu-preselect 'first
      corfu-popupinfo-mode -1)

(map!
 :desc "Transient"
 :n "C-f" #'transient-scroll-up
 :n "C-b" #'transient-scroll-down
 :n "C-j" #'transient-scroll-up
 :n "C-k" #'transient-scroll-down)


(map! :leader
      :n "e s" (lambda () (interactive)
                 (pyvenv-activate "~/temp-env/.venv")
                 )
      :n "c l" #'aider-transient-menu)

;; (map! :leader
;;  :n "c f" #'eglot-format-buffer
;; )

(load! "jj.el")
(load! "ai.el")

(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(defun +ui/increase-opacity ()
  (interactive)
  (let* ((current (or (car (frame-parameter nil 'alpha)) 100))
         (new (min 100 (+ current 5))))
    (set-frame-parameter nil 'alpha (cons new new))))

(defun +ui/decrease-opacity ()
  (interactive)
  (let* ((current (or (car (frame-parameter nil 'alpha)) 100))
         (new (max 0 (- current 5))))
    (set-frame-parameter nil 'alpha (cons new new))))

(map! :leader
      :desc "Increase transparency" "t +" #'+ui/decrease-opacity
      :desc "Project diagnostics" "c X" #'flymake-show-project-diagnostics
      :desc "Project diagnostics" "c x" #'flymake-show-buffer-diagnostics
      :desc "Decrease transparency" "t -" #'+ui/increase-opacity)

(map!
 :desc "Rename variable" :n "grn" #'eglot-rename
 :desc "Find references" :n "grr" #'eglot-find-references
 :desc "Code actions" :n "gra" #'eglot-code-actions
 :desc "Type Definitions" :n "grt" #'eglot-find-typeDefinition
 )

;; (use-package! lsp-mode
;;   :hook '(
;;           (typescript-t-mode . lsp-deferred)
;;           (tsx-ts-mode . lsp-deferred))
;;
;;

(after! keycast ;; same as (with-eval-after-load 'keycast
  (add-to-list 'global-mode-string '("" mode-line-keycast)))



(after! keycast
  )

(use-package! keycast
  :hook (after-init . keycast-mode)
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-modeline)."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(use-package! spacious-padding
  :if (display-graphic-p)
  :after doom-modeline
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 8
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           ;; :fringe-width 8
           ))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-frame-lines
        `( :mode-line-active "#FFFFFF"
           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode)
  )

;; (add-to-list 'major-mode-remap-alist
;;              '(typescript-mode . typescript-ts-mode))

;; (add-to-list 'auto-mode-alist
;;              '("\\.ts\\'" . typescript-ts-mode)
;;              '("\\.tsx\\'" . tsx-ts-mode))

;; (add-hook 'lsp-mode-hook
;;           (lambda ()
;;             (when (and (eq major-mode 'typescript-ts-mode)
;;                        (not (lsp-find-workspace 'typescript-ts-mode nil)))
;;               (lsp))))
;;
;; BUFFER PLACEMENTS
(defun my/focus-buffer (window)
  (select-window window))

(add-to-list 'display-buffer-alist
             '("^\\*eldoc\\*"
               (display-buffer-at-bottom)
               (display-buffer-reuse-mode-window)
               (body-function . my/focus-buffer)
               (window-height . 10)))
(add-to-list 'display-buffer-alist
             '("^\\*Flutter\\*"
               (display-buffer-at-bottom)
               (display-buffer-reuse-mode-window)
               (body-function . my/focus-buffer)
               (window-height . 10)))
(add-to-list 'display-buffer-alist
             '("\\*Flymake diagnostics for \*"
               (display-buffer-at-bottom)
               (display-buffer-reuse-mode-window)
               (body-function . my/focus-buffer)
               (window-height . 10)))

;; SET SERVER PROGRAMS
(with-eval-after-load 'eglot
  (dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode))
    (add-to-list 'eglot-server-programs
                 `(,m .
                   ("vtsls" "--stdio"))))
  ;; Remove legacy tsserver if desired
  (setq eglot-server-programs
        (assq-delete-all 'typescript-ts-mode eglot-server-programs))

  (setq-default eglot-workspace-configuration
                '((vtsls
                   . ((completeFunctionCalls . t)
                      (typescript . ((updateImportsOnFileMove . ((enabled . "always")))
                                     (suggest . ((completeFunctionCalls . t)))
                                     (inlayHints . ((parameterNames . ((enabled . "literals")
                                                                       (suppressWhenArgumentMatchesName . nil)))
                                                    (parameterTypes . ((enabled . t)))
                                                    (variableTypes . ((enabled . nil)))
                                                    (propertyDeclarationTypes . ((enabled . t)))
                                                    (functionLikeReturnTypes . ((enabled . t)))
                                                    (enumMemberValues . ((enabled . t))))))))))))


(add-to-list 'load-path doom-private-dir)

(require 'ez-flutter)
;; ~/.doom.d/config.el
