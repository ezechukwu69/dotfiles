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
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 18 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 18)
      doom-big-font (font-spec :family "Iosevka Nerd Font" :size 25))

(after! doom-themes
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t))


(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq-default line-spacing 3)

(setq org-agenda-files (directory-files-recursively "~/org/agenda" "\\.org$"))
(setq org-hide-emphasis-markers t)
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(defun set-vivendi-override()
  (setq modus-themes-vivendi-color-overrides
        '((bg-header . "#2F3541")   ; Darker header background
          (bg-hl-line . "#2B303B")  ; Darker highlight line background
          (bg-inactive . "#1F242D") ; Darker inactive background
          (bg-main . "#0F1215")))

  (custom-set-faces
   '(line-number ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(tab-bar ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(tab-line ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(centaur-tabs-selected ((t (:box nil :foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(centaur-tabs-default ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(tab-bar-tab-group-current ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(tab-bar-tab ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(doom-modeline-buffer-file ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(doom-modeline-buffer-path ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(doom-modeline ((t (:foreground "#4C566A" :background "#0F1215"))))  ; Darker non-current line number
   '(mode-line ((t (:box nil height 2 :line-height 1))))     ; Remove border for the active modeline
   '(hl-line ((t (:background "#1C1F24")))) ; Very dark background for hl-line
   '(mode-line-inactive ((t (:box nil height 2 :line-height 1)))) ; Remove border for the inactive modeline
   '(line-number-current-line ((t (:foreground "#D08770" :background nil))))) ; Darker current line number
  )

;; (set-vivendi-override)

(setq doom-theme 'doom-monokai-spectrum)

(setq treesit-font-lock-level 4)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(map! :leader
      :desc "Toggle truncate lines"
      "t T" #'toggle-truncate-lines)

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
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! tree-sitter
  :config
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'tree-sitter-after-on-hook #'rainbow-delimiters-mode)
(add-hook 'tree-sitter-after-on-hook #'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'font-lock-mode)
(add-hook 'prog-mode-hook #'highlight-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(set-email-account!
 "gmail"
 '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/[Gmail]/Bin")
   (smtpmail-smtp-user     . "ezechukwu69@gmail.com"))
 t)

(setq mu4e-get-mail-command "mbsync gmail"
      ;; get emails and index every 5 minutes
      mu4e-update-interval 300
      ;; send emails with format=flowed
      mu4e-compose-format-flowed t
      ;; no need to run cleanup after indexing for gmail
      mu4e-index-cleanup nil
      mu4e-index-lazy-check t
      ;; more sensible date format
      mu4e-headers-date-format "%d.%m.%y")


;; we recommend using use-package to organize your init.el
;; (use-package codeium
;;     ;; if you use straight
;;     ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
;;     ;; otherwise, make sure that the codeium.el file is on load-path

;;     :init
;;     ;; use globally
;;     (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;     ;; or on a hook
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local completion-at-point-functions
;;     ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;     ;; an async company-backend is coming soon!

;;     ;; codeium-completion-at-point is autoloaded, but you can
;;     ;; optionally set a timer, which might speed up things as the
;;     ;; codeium local language server takes ~0.2s to start up
;;     ;; (add-hook 'emacs-startup-hook
;;     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;     ;; :defer t ;; lazy loading, if you want
;;     :config
;;     (setq use-dialog-box nil) ;; do not use popup boxes

;;     ;; if you don't want to use customize to save the api-key
;;     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;     ;; get codeium status in the modeline
;;     (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;     ;; alternatively for a more extensive mode-line
;;     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;     (setq codeium-api-enabled
;;         (lambda (api)
;;             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;     ;; you can also set a config for a single buffer like this:
;;     ;; (add-hook 'python-mode-hook
;;     ;;     (lambda ()
;;     ;;         (setq-local codeium/editor_options/tab_size 4)))

;;     ;; You can overwrite all the codeium configs!
;;     ;; for example, we recommend limiting the string sent to codeium for better performance
;;     (defun my-codeium/document/text ()
;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;     ;; if you change the text, you should also change the cursor_offset
;;     ;; warning: this is measured by UTF-8 encoded bytes
;;     (defun my-codeium/document/cursor_offset ()
;;         (codeium-utf8-byte-length
;;             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;     (setq codeium/document/text 'my-codeium/document/text)
;;     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; (after! codeium
;;   ;; Bind `C-]` in normal mode to trigger codeium-complete
;;   (map! :n "C-]" #'codeium-complete)

;;   ;; Optionally, bind `C-]` in insert mode if desired
;;   (map! :i "C-]" #'codeium-complete))



(setq lsp-dart-dap-flutter-hot-reload-on-save t)
(setq select-enable-clipboard t)

(after! vertico
  (setq vertico-multiform-commands
        '((execute-extended-command (vertico-multiform-annotate . builtin)))
        vertico-cycle t))

(after! corfu
  (setq corfu-preselect 'first))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (use-package! supermaven
;;   :hook '(prog-mode . supermaven-mode))

;; (setq supermaven-ignore-filetypes '("org" "txt"))
;; (setq supermaven-disable-inline-completion nil)
;; (setq supermaven-keymaps
;;       '((accept-suggestion . "C-]")
;;         (clear-suggestion . "C-[")
;;         (accept-word . "C-j")))
;; (setq supermaven-log-level 'debug)
