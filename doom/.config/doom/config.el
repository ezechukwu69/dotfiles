;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
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
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 15 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
(map! :leader :desc "Navigate to right window" :n "C-l" #'evil-window-right)
(map! :leader :desc "Navigate to left window" :n "C-h" #'evil-window-left)
(map! :leader :desc "Navigate to down window" :n "C-j" #'evil-window-down)
(map! :leader :desc "Navigate to up window" :n "C-k" #'evil-window-up)
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

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

;; (add-hook 'dart-mode-hook
;;           (lambda ()
;;             (when (treesit-ready-p 'dart)
;;               (treesit-hl-mode))))

;; (setq major-mode-remap-alist
;;       '(
;;         (yaml-mode . yaml-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         ))

(use-package aider
  :config
  (setq aider-args
        '("--model" "gemini/gemini-2.0-flash"
          "--vim"
          "--no-attribute-author"
          "--no-attribute-committer"
          "--no-attribute-commit-message-author"
          "--no-attribute-commit-message-committer"
          "--watch-files"
          "--no-auto-commits"
          "--no-dirty-commits"
          "--edit-format" "diff-fenced"
          "--no-auto-lint"
          "--architect"
          )))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode  2))
  (add-to-list 'copilot-indentation-alist '(org-mode  2))
  (add-to-list 'copilot-indentation-alist '(text-mode  2))
  (add-to-list 'copilot-indentation-alist '(closure-mode  2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2)))

(use-package! gptel
  :config
  ;; OPTIONAL configuration
  (setq
   ;; gptel-model 'gemini-2.0-flash
   ;; gptel-model 'gemini-2.5-flash-preview-04-17
   gptel-model 'gemini-2.5-pro-preview-05-06
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (getenv "GEMINI_API_KEY")
                   :stream t)))

(use-package! emigo
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key (getenv "OPENROUTER_API_KEY")))

;; (use-package! supermaven
;;   :hook (prog-mode . supermaven-mode))
;; we recommend using use-package to organize your init.el
;; (use-package! codeium
;;   ;; if you use straight
;;   ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;   ;; otherwise, make sure that the codeium.el file is on load-path

;;   :init
;;   ;; use globally
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   ;; or on a hook
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;   ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions
;;   ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;   ;; an async company-backend is coming soon!

;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; :defer t ;; lazy loading, if you want
;;   :config
;;   (setq use-dialog-box nil) ;; do not use popup boxes

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))

;;   ;; You can overwrite all the codeium configs!
;;   ;; for example, we recommend limiting the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(setq gptel-functions
      '(("runShellCommand"
         :description "Run a shell command"
         :parameters (:type "object"
                      :properties ((cmd :type "string")))
         :function (lambda (args)
                     (shell-command-to-string (alist-get 'cmd arg))))))
