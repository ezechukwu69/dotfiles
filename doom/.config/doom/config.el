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
(setq doom-font (font-spec :family "Monaspace Argon" :size 14 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 13))

(setq-default line-spacing 6)

(setq org-agenda-files (directory-files-recursively "~/org/agenda" "\\.org$"))
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

(setq doom-theme 'doom-material-dark)

(setq treesit-font-lock-level 4)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
