;;; ../dotfiles/doom/.doom.d/ez-flutter.el -*- lexical-binding: t; -*-
(defvar ez/flutter--advice-installed nil
  "Track whether save-buffer advice is installed.")


(defun ez/flutter--advice-save-buffer (orig-fun &rest args)
  "Advice function to trigger hot reload after saving."
  (let ((result (apply orig-fun args)))
    ;; Only trigger if the current buffer has hot reload mode enabled
    (when (and buffer-file-name
               (string-match-p "\\.dart\\'" buffer-file-name)
               (bound-and-true-p ez/flutter-hot-reload-mode))
      (run-with-timer 0.1 nil #'ez/flutter-reload))
    result))

(defun ez/flutter--ensure-global-advice ()
  "Ensure global `save-buffer advice` is installed."
  (unless ez/flutter--advice-installed
    (advice-add 'save-buffer :around #'ez/flutter--advice-save-buffer)
    (setq ez/flutter--advice-installed t)))

(defun ez/flutter--remove-global-advice ()
  "Remove global save-buffer advice if no buffers use it."
  (when ez/flutter--advice-installed
    ;; Check if any buffer still has hot reload mode enabled
    (let ((has-active-mode nil))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p ez/flutter-hot-reload-mode)
            (setq has-active-mode t))))
      (unless has-active-mode
        (advice-remove 'save-buffer #'ez/flutter--advice-save-buffer)
        (setq ez/flutter--advice-installed nil)))))

(defun ez/flutter-restart ()
  "Trigger a hot restart of the Flutter application."
  (interactive)
  (when-let ((conn (dape--live-connection 'last t)))
    (dape-request conn "hotRestart" nil)))

(defun ez/flutter-reload ()
  "Trigger a hot restart of the Flutter application."
  (interactive)
  (when-let ((conn (dape--live-connection 'last t)))
    (dape-request conn "hotReload" nil)))

(defun ez/flutter-start ()
  "Start the Flutter application in a new process."
  (interactive)
  (dape `(modes (dart-mode dart-ts-mode)
          ;; Modify this to your preferred path
          command "flutter"
          command-args ("debug_adapter")
          command-cwd ,(projectile-project-root)
          ;; important Dape fields:
          :type "dart"
          :program "lib/main.dart"
          :request "launch"
          :program "lib/main.dart"
          :cwd ".")))


(define-minor-mode ez/flutter-hot-reload-mode
  "Minor mode to enable hot reload on save for Flutter projects."
  :lighter " 🔥Flutter"
  :group 'ez-flutter
  (if ez/flutter-hot-reload-mode
      (progn
        (message "🔥 ENABLING Flutter hot reload mode in buffer: %s" (buffer-name))
        ;; Use after-save-hook for buffer-local behavior
        (add-hook 'after-save-hook #'ez/flutter-reload nil t)
        (ez/flutter--ensure-global-advice)
        (message "🔥 Flutter hot reload on save: ENABLED"))
    (progn
      (message "❌ DISABLING Flutter hot reload mode")
      (remove-hook 'after-save-hook #'ez/flutter-reload t)
      (ez/flutter--remove-global-advice)
      (message "❌ Flutter hot reload on save: DISABLED"))))


(defvar ez/flutter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f r") #'ez/flutter-reload)
    (define-key map (kbd "C-c f R") #'ez/flutter-restart)
    (define-key map (kbd "C-c f s") #'ez/flutter-start)
    map)
  "Keymap for ez-flutter commands.")

(define-minor-mode ez/flutter-mode
  "Minor mode for Flutter development commands."
  :lighter " Flutter"
  :keymap ez/flutter-mode-map
  :group 'ez-flutter)


(defun ez/flutter--setup-dart-mode ()
  "Setup Flutter hot reload mode for Dart files in Flutter projects."
  (when (locate-dominating-file default-directory "pubspec.yaml")
    (ez/flutter-hot-reload-mode 1)
    (ez/flutter-mode 1)))

;; Ensure .dart files trigger dart-mode
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;; Hook setup for Dart source files
(add-hook 'dart-mode-hook #'ez/flutter--setup-dart-mode)

(with-eval-after-load 'dape
  (setq dape-info-buffer-window-groups
        '((dape-info-sources-mode dape-info-watch-mode)
          (dape-info-scope-mode dape-info-stack-mode dape-info-modules-mode)
          (dape-info-breakpoints-mode dape-info-threads-mode)))
  )

(provide 'ez-flutter)
;;; ez-flutter.el ends here
