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
(setq doom-theme 'doom-tomorrow-night)

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
                     ""))

  (require 'aider-doom))


(use-package! expand-region
  :bind
  ("M-=" . er/expand-region)
  ("M--" . er/contract-region))

(use-package! mcp
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/lizqwer/MyProject/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))

  :config
  (require 'mcp-hub)
  (require 'gptel-integrations)
  :hook
  (after-init . mcp-hub-start-all-server))


(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(setq which-key-use-C-h-commands t
      which-key-paging-prefixes '("C-x")
      which-key-paging-key "<f5>"
      evil-snipe-scope 'buffer
      avy-all-windows t)

(map! :leader
      :desc "Show project diagnostics"
      :n "c X" #'flymake-show-project-diagnostics)


(add-hook 'dart-mode-hook
          (lambda ()
            (setq lsp-dart-dap-flutter-hot-reload-on-save t)))



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
      :n "c l" #'aider-transient-menu)

;; (map! :leader
;;  :n "c f" #'eglot-format-buffer
;; )

(load! "jj.el")
(load! "ai.el")
