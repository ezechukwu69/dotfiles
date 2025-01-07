(electric-pair-mode 1)
(setq eldoc-echo-area-use-multiline-p t)
(global-eldoc-mode 1)
(setq max-mini-window-height 3)
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

(use-package toc-org
   :ensure t
   :commands toc-org-enable
   :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right nil)
  (setq evil-split-window-below nil)
  (evil-mode))
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard magit dired ibuffer Occur))
  (evil-collection-init))
(use-package evil-tutor :ensure t)

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-create-definer eze/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer eze/non-leader-keys
    :states '(normal)
    :keymaps 'override)

  (eze/non-leader-keys
    "g c c" '(comment-line :wk "Comment line"))

  
  (eze/non-leader-keys
    "g S"  '(consult-ripgrep :wk "Find workspace symbols")
    "g s"  '(consult-imenu :wk "")
    "g y" '(eglot-find-typeDefinition :which-key "go to type definition")
    "g D" '(eglot-find-declaration :which-key "go to declaration")
    "g I" '(eglot-find-implementation :which-key "go to implementation")
    "g r"  '(xref-find-references :which-key "find references")
    "g ."  '(eglot-code-actions :which-key "code actions")
    "K"  '(eglot-hover :which-key "show hover info")
    )

  (eze/leader-keys
    "c a" '(eglot-code-actions :wk "Code action")
    "c S" '(consult-eglot-symbols :wk "Code action")
    "c s" '(consult-imenu :wk "Code action")
    "c f"  '(eglot-format-buffer :which-key "format document")
    "c r"  '(eglot-rename :which-key "rename symbol")
  )

(eze/leader-keys
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Open private config")
  "l c" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload config"))

(eze/leader-keys
  "b" '(:ignore t :wk "buffer")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "IBuffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer"))


(eze/leader-keys
  "g" '(:ignore t :wk "GIT")
  "g g" '(magit :wk "Open magit"))

(eze/leader-keys
  "e" '(:ignore t :wk "Evaluate")
  "e b" '(eval-buffer :wk "Eval buffer")
  "e d" '(next-defun :wk "Evaluate defun at point")
  "e e" '(evaluate-expression :wk "Eval Expression")
  "e r" '(eval-region :wk "Eval region")
  "e l" '(eval-last-sexp :wk "Eval Elisp expression before point"))

(eze/leader-keys
  "f" '(:ignore t :wk "File/Find")
  "f f"  '(find-file :wk "Find File")
  "SPC"  '(projectile-find-file :wk "Project Find File"))

(eze/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t l"  '(display-line-numbers-mode :wk "Describe function")
  "t t"  '(visual-line-mode :wk "Toggle truncated line"))


(eze/leader-keys
  "p" '(:ignore t :wk "Project")
  "p A"  '(projectile-add-known-project :wk "Projectile Add projectile")
  "p s"  '(projectile-switch-project :wk "Projectile switch"))

(eze/leader-keys
  "h" '(:ignore t :wk "Help")
  "h v"  '(describe-variable :wk "Describe function")
  "h f"  '(describe-function :wk "Describe variable"))
)

(set-face-attribute 'default nil
   :font "SpaceMono Nerd Font"
   :height 110
   :weight 'bold)
(set-face-attribute 'variable-pitch nil
   :font "SpaceMono Nerd Font"
   :height 120
   :weight 'medium)
(set-face-attribute 'fixed-pitch nil
   :font "SpaceMono Nerd Font"
   :height 110
   :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil
   :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
   :slant 'italic)
;;(add-to-list 'default-frame-alist '(font . "SpaceMono Nerd Font-11"))
(setq-default line-spacing 0.12)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-dracula") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-window-location 'bottom)
  (setq which-key-max-description-length 50) ;; Increase the max description length if needed
  (setq which-key-allow-imprecise-window-fit t) ;; Allow imprecise fits
  (setq which-key-side-window-max-height 0.4) ;; Max height of the Which Key window
  (setq which-key-side-window-min-height 0.3) ;; Max height of the Which Key window
  (setq which-key-side-window-max-width 0.5) ;; Max width of the Which Key window
  (setq which-key-popup-type 'side-window) ;; Use a side-window popup
  (setq which-key-sort-order 'which-key-key-order-alpha) ;; Optional: Alphabetical sorting
  ;; Wrap text
  ;; (setq which-key-show-prefix 'top) ;; Show the prefix at the top
  )

(defun my/which-key-disable-line-numbers ()
  "Disable line numbers in the Which Key buffer."
  (when (derived-mode-p 'special-mode)
    (display-line-numbers-mode -1)))

(add-hook 'which-key-show-popup-hook #'my/which-key-disable-line-numbers)

(defun my/which-key-enable-wrap ()
  "Enable line wrapping for the Which Key buffer."
  (with-current-buffer (get-buffer which-key--buffer)
    (setq-local truncate-lines nil) ;; Disable line truncation
    (visual-line-mode 1))) ;; Enable visual line wrapping

(add-hook 'which-key-show-popup-hook #'my/which-key-enable-wrap)

;; (use-package supermaven
;;   :ensure t
;;   (:host github :repo "crazywolf132/supermaven.el")
;;   :config
;;   ;;(setq supermaven-ignore-filetypes '("org" "txt"))
;;   ;;(setq supermaven-disable-inline-completion nil)
;;   (setq supermaven-keymaps
;;         '((accept-suggestion . "TAB")
;;           (clear-suggestion . "C-]")
;;           (accept-word . "C-j")))
;;   (setq supermaven-log-level 'debug)
;;   ;; Package-specific configuration here
;;   :hook
;;     (prog-mode . supermaven-mode)
;;   )

;; we recommend using use-package to organize your init.el
(use-package codeium
    :ensure t
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path
    (:host github :repo "Exafunction/codeium.el")

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package all-the-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package helm
  :ensure t
  :config)

(use-package eglot
  :ensure t)

(use-package consult
  :after vertico
  :ensure t)

(setq xref-show-definitions-function #'consult-xref
      xref-show-xrefs-function #'consult-xref)

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t          ;; Enable auto-popup
        corfu-auto-delay 0.2  ;; Delay in seconds before popup
        corfu-cycle t        ;; Enable cycling
        corfu-auto-prefix 1)
  (global-corfu-mode)
  :bind (:map corfu-map             ;; Keybindings for Corfu
              ([tab] . corfu-next)  ;; Move to the next suggestion
              ([backtab] . corfu-previous) ;; Move to the previous suggestion
              ("<return>" . corfu-insert) ;; Accept the selected suggestion
              ("RET" . corfu-insert)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ;; Align icons with corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package eglot
  :ensure t
  :hook ((prog-mode . eglot-ensure))    ;; Example for another language (Go)
  :config
  ;; (add-to-list 'eglot-server-programs '((javascript-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  ;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  )  ;; Optional: Customize server capabilities
(add-hook 'prog-mode-hook 'imenu-add-menubar-index)
(setq eglot-hover-eldoc-function #'eldoc-hover)

(setq treesit-language-source-alist
      '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
  	(jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (java . ("https://github.com/tree-sitter/tree-sitter-java"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (scss . ("https://github.com/tree-sitter/tree-sitter-scss"))
        (swift . ("https://github.com/tree-sitter/tree-sitter-swift"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (objc . ("https://github.com/tree-sitter/tree-sitter-objc"))
        (lua . ("https://github.com/tree-sitter/tree-sitter-lua"))
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
        (kotlin . ("https://github.com/tree-sitter/tree-sitter-kotlin"))
        (r . ("https://github.com/tree-sitter/tree-sitter-r"))
        (markdown . ("https://github.com/tree-sitter/tree-sitter-markdown"))
        (elixir . ("https://github.com/tree-sitter/tree-sitter-elixir"))
        (dart . ("https://github.com/tree-sitter/tree-sitter-dart"))
        (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
        (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
        (fsharp . ("https://github.com/tree-sitter/tree-sitter-fsharp"))
        (viml . ("https://github.com/tree-sitter/tree-sitter-viml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (yaml . ("https://github.com/tree-sitter/tree-sitter-yaml"))
        (sql . ("https://github.com/tree-sitter/tree-sitter-sql"))
        (perl . ("https://github.com/tree-sitter/tree-sitter-perl"))
        (tex . ("https://github.com/tree-sitter/tree-sitter-tex"))
        (clojure . ("https://github.com/tree-sitter/tree-sitter-clojure"))
        (graphql . ("https://github.com/tree-sitter/tree-sitter-graphql"))
        (zig . ("https://github.com/tree-sitter/tree-sitter-zig"))
        (vhdl . ("https://github.com/tree-sitter/tree-sitter-vhdl"))
        (racket . ("https://github.com/tree-sitter/tree-sitter-racket"))
        (json5 . ("https://github.com/tree-sitter/tree-sitter-json5"))
        (haxe . ("https://github.com/tree-sitter/tree-sitter-haxe"))
        (nix . ("https://github.com/tree-sitter/tree-sitter-nix"))
        (pony . ("https://github.com/tree-sitter/tree-sitter-pony"))
        (solidity . ("https://github.com/tree-sitter/tree-sitter-solidity"))
        (vlang . ("https://github.com/tree-sitter/tree-sitter-vlang"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml"))
        (vala . ("https://github.com/tree-sitter/tree-sitter-vala"))
        (sed . ("https://github.com/tree-sitter/tree-sitter-sed"))
        (rts . ("https://github.com/tree-sitter/tree-sitter-rts"))))
(setq temporary-file-directory "~/tmp/")

;; TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json5\\'" . json-ts-mode))

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-ts-mode))

;; Java
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

;; C, C++, Objective-C
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-ts-mode))

;; C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))

;; Go
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-ts-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . css-ts-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-ts-mode))

;; Shell Scripts
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-ts-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-ts-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;; Perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-ts-mode))
(add-to-list 'auto-mode-alist '("\\.pm\\'" . cperl-ts-mode))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))

;; Swift
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-ts-mode))

;; Kotlin
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-ts-mode))

;; Haskell
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))

;; Makefiles
(add-to-list 'auto-mode-alist '("Makefile\\'" . makefile-ts-mode))
(add-to-list 'auto-mode-alist '("makefile\\'" . makefile-ts-mode))

;; Vim Scripts
(add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-ts-mode))

;; Lisp
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-ts-mode))

;; JSON-LD
(add-to-list 'auto-mode-alist '("\\.jsonld\\'" . json-ts-mode))

;; SQL
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-ts-mode))

;; Text Files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-ts-mode))


;; Check if the directory exists, and create it if it doesn't
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory t))

(setq treesit-work-dir "~/tmp/treesit/")

(setq tree-sitter-font-lock-level 3)  ;; Use high-level font-locking
(setq treesit-font-lock-level 4)  ;; Use high-level font-locking
(add-hook 'tree-sitter-after-on-hook #'font-lock-mode)
(add-hook 'js-mode-hook (lambda () (font-lock-mode -1)))
(add-hook 'typescript-mode-hook (lambda () (font-lock-mode -1)))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  ;; (setq ibuffer-use-other-window t) ;; Open ibuffer in another window
  (setq ibuffer-auto-mode t)       ;; Automatically update ibuffer
  (add-to-list 'ibuffer-filter-groups
               '("Other" (predicate . (lambda (buf)
                                        (with-current-buffer buf
                                          (not (projectile-project-p)))))))
  (setq ibuffer-default-sorting-mode 'alphabetic)

  (defun my/projectile-add-current-project-on-file-open ()
    "Automatically add the current file's project to Projectile when opening a file."
    (when (and (projectile-project-p) ;; Check if the file belongs to a project
               (not (projectile-project-p (projectile-project-root)))) ;; Check if it's already a known project
      (projectile-add-known-project (projectile-project-root)))) ;; Add it to the known projects

  (add-hook 'find-file-hook #'my/projectile-add-current-project-on-file-open)

  )

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
          (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file)))
  )
