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
  (setq visible-bell t)

  ;; Key bindings
  (require 'bind-key)

  ;;; macOS bindings
  (when (string= "darwin" system-type)
    ;; Move through windows using meta-<arrows>
    (global-set-key (kbd "s-<right>") 'windmove-right)
    (global-set-key (kbd "s-<left>")  'windmove-left)
    (global-set-key (kbd "s-<up>")    'windmove-up)
    (global-set-key (kbd "s-<down>")  'windmove-down)
    ;; Do not close Emacs on Command-q
    (global-set-key (kbd "s-q") nil)
    ;; Do not display print dialog
    (global-set-key (kbd "s-p") nil))

  ;;; Glorious package to let you know what binding are available
  (use-package which-key
    :defer nil
    :diminish which-key-mode
    :config
    (which-key-mode))

  ;; Visual settings
  ;;; Remove sual clutter
  (when (display-graphic-p)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (global-hl-line-mode -1))

  ;;; Use a theme
  (load-theme 'wheatgrass)

  ;;; Default font to Go Mono, if available
  ;;; https://blog.golang.org/go-fonts
  (condition-case nil
      (set-face-attribute 'default nil :family "Go Mono" :height 140))

  ;;; Disable startup screen
  (setq startup-screen-inhibit-startup-screen t
        inhibit-startup-screen t)

  ;;; Misc settings
  (setq query-replace-highlight t	 ; Highlight matches during query replacement
      search-highlight t           ; Highlight current search match
      column-number-mode t         ; Column number on status bar
      size-indication-mode t       ; Show buffer size
      blink-cursor-mode t          ; Blink the cursor
      frame-title-format "%b (%f)" ; Frame title format
      )

  ;;; http://pragmaticemacs.com/emacs/adaptive-cursor-width/
  ;;; make cursor the width of the character it is under
  ;;; i.e. full width of a TAB
  (setq x-stretch-cursor t)

  ;; Set GC threshold to 1GB
  (setq gc-cons-threshold (* 1000 1000))
  ) ;; End prevent of special filename parsing
