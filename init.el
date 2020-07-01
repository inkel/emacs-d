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

  ;;; Delete highlighted region
  (delete-selection-mode t)

  ;;; Use y/n instead of yes/no for questions
  (defalias 'yes-or-no-p 'y-or-n-)

  ;;; Use system clipboard for copy/paste
  (setq x-select-enable-clipboard t)

  ;;; Reload files when changed
  (global-auto-revert-mode 1)

  ;;; Disable backups
  (setq backup-inhibited t)

  ;;; Tab either indents or complete
  (setq tab-always-indent 'complete)

  ;;; Use UTF-8 by default
  (prefer-coding-system        'utf-8)
  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)

  ;;; Indentation
  (setq standard-indent 2)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)

  ;; Key bindings
  (require 'bind-key)

  ;;; macOS bindings
  (use-package windmove ;; For moving across windows
    :ensure t
    :config (windmove-default-keybindings 'super)
    (setq windmove-wrap-around t))

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

  ;;; Do not send to background/minimize
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x C-z") nil)

  ;;; Glorious package to let you know what binding are available
  (use-package which-key
    :defer nil
    :diminish which-key-mode
    :config
    (which-key-mode))

  ;;; I love this package, even when I only use one function
  (use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-S->" . mc/unmark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C->" . mc/mark-all-like-this)
           ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

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

  ;;; Make it easy to identify each open file
  (use-package uniquify
    :defer 1
    :ensure nil
    :custom
    (uniquify-after-kill-buffer-p t)
    (uniquify-buffer-name-style 'post-forward)
    (uniquify-strip-common-suffix t))

  ;; Magit - Enough reason to use Emacs
  (use-package magit
    :bind (("C-x g" . magit-status))
    :config
    (progn
      (defun inkel/magit-log-edit-mode-hook ()
        (setq fill-column 72)
        (flyspell-mode t)
        (turn-on-auto-fill))
      (add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)
      (setq vc-handled-backends (delq 'Git vc-handled-backends))
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))))

  ;; Set GC threshold to 1GB
  (setq gc-cons-threshold (* 1000 1000))
  ) ;; End prevent of special filename parsing
