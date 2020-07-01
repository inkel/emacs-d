;; Raise GC threshold to maximum available
(setq gc-cons-threshold most-positive-fixnum)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Prevent special filename parsing
(let ((file-name-handler-alist nil))

  ;; Custom variables
  (setq custom-file "~/.emacs.d/custom.el")
  (if (file-exists-p custom-file)
      (load custom-file))

  ;; Package management
  (customize-set-variable 'package-archives
                          '(("gnu"       . "https://elpa.gnu.org/packages/")
                            ("marmalade" . "https://marmalade-repo.org/packages/")
                            ("melpa"     . "https://melpa.org/packages/")))

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))

  (require 'use-package)

  (customize-set-variable 'use-package-always-ensure t)
  (customize-set-variable 'use-package-always-defer t)
  (customize-set-variable 'use-package-verbose nil)

  ;; Loading compiled files
  (customize-set-variable 'load-prefer-newer t)

  ;; Misc settings

  ;;; Paste where cursor is
  (customize-set-variable 'mouse-yank-at-point t)

  ;;; Show line numbers
  (use-package display-line-numbers
    :defer nil
    :ensure nil
    :config (global-display-line-numbers-mode))

  ;;; Trailing whitespace is bad and you should feel bad
  (customize-set-variable 'show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;;; Matching parenthesis
  (show-paren-mode)

  ;;; Fuck tabs!
  (customize-set-variable 'indent-tabs-mode nil)

  ;;; Those pesky backups files…
  (customize-set-variable 'backup-directory-alist
                          `(("." . ,(concat user-emacs-directory "backups"))))

  ;; Adds a new line at the last line
  (setq require-final-newline t)

  ;;; Sound off
  (setq ring-bell-function 'ignore)

  ;; Set GC threshold to 1GB
  (setq gc-cons-threshold (* 1000 1000))
  ) ;; End prevent of special filename parsing
