;; Disable startup screen
(setq startup-screen-inhibit-startup-screen t
      inhibit-startup-screen t)

;; Default font
(condition-case nil
    (set-face-attribute 'default nil :family "Monaco" :height 140))

(cond ((eq system-type 'darwin)
       (set-default-font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :family "Droid Sans Mono" :height 100)))

;; Packages
(require 'package)

;;; Do not activate installed packages when Emacs starts.
(setq package-enable-at-startup nil)

;;; Additional sources for packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;;; Load and activate packages
(package-initialize)

;;; This article changed my life
;;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;; Fix $PATH in OSX Emacs
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;;;; Company. Auto-completion.
(use-package company
  :ensure t
  :bind (("C-<tab>" . company-complete))
  :config
  (global-company-mode))

;;;; magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (defun inkel/magit-log-edit-mode-hook ()
      (setq fill-column 72)
      (flyspell-mode t)
      (turn-on-auto-fill))
    (add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))))

;;;; Go
(use-package go-mode
  :ensure t
  :bind (("C-c C-r" . go-remove-unused-imports)
	 ("C-c C-g" . go-goto-imports)
	 ("C-c C-f" . gofmt)
	 ("C-c C-k" . godoc))
  :config
  (progn
    (setenv "GOPATH" "/home/inkel/dev/go")
    (setq gofmt-command "~/dev/go/bin/goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package go-guru
  :ensure t
  :config (progn
            (setenv "GOPATH" "/home/inkel/dev/go")
            (setq go-guru-command "/home/inkel/dev/go/bin/guru")
            (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)))

(use-package golint
  :load-path "~/dev/go/src/github.com/golang/lint/misc/emacs")

(use-package company-go
  :ensure t
  :config
  (progn
    (setq company-go-gocode-command "~/dev/go/bin/gocode")
    (defun inkel/company-go-hook ()
      (set (make-local-variable 'company-backends) '(company-go))
      (company-mode t))
    (add-hook 'go-mode-hook 'inkel/company-go-hook)))

;;;; Can't say how manyn times this saved me. SO useful.
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;; Use unique buffer names
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward
	uniquify-strip-common-suffix nil
	uniquify-after-kill-buffer-p t))

;;;; Highlight parenthesis/brackets
(use-package paren
  :init
  (setq show-paren-style 'mixed)
  :config
  (show-paren-mode t))

;;; M-x on steroids
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :config (setq search-default-mode nil))
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))
(use-package ivy
  :ensure t
  :config (progn
            (setq ivy-use-virtual-buffers t
                  ivy-height 10
                  ivy-count-format "(%d/%d) ")
            (ivy-mode 1)))

;; Load custom settings if present
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Visual settings
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode -1)
  (load-theme 'whiteboard))

(setq query-replace-highlight t		; Highlight matches during query replacement
      search-highlight t		; Highlight current search match
      column-number-mode t		; Column number on status bar
      size-indication-mode t		; Show buffer size
      blink-cursor-mode t		; Blink the cursor
      frame-title-format "%b (%f)"	; Frame title format
      )

;; Daemon for emacsclient
(use-package server
  :config
  (server-start))

;; Ruby Ruby Ruby... ooooohhh ooohh ooooh
(use-package ruby-mode
  :mode (("\\.rake$"    . ruby-mode)
	 ("\\.ru$"      . ruby-mode)
	 ("\\.gemspec$" . ruby-mode)
	 ("Gemfile"     . ruby-mode)
	 ("Thorfile"    . ruby-mode)
	 ("Guardfile"   . ruby-mode)
	 ("Rakefile"    . ruby-mode)
	 ("\\.builder$" . ruby-mode)
	 ("\\.god$"     . ruby-mode)))

;; Docker
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; Edit nginx files
(use-package nginx-mode :ensure t)

;; OSX bindings
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

;; Do not send to background/minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; ibuffer
(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

;; Analyze script hash-bang and mark it as executable if possible on
;; first save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Delete highlighted region
(delete-selection-mode t)

;; Use y/n instead of yes/no for questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Adds a new line at the last line
(setq require-final-newline t)

;; Use system clipboard for copy/paste
(setq x-select-enable-clipboard t)

;; Reload files when changed
(global-auto-revert-mode 1)

;; Disable backups
(setq backup-inhibited t)

;; Tab either indents or complete
(setq tab-always-indent 'complete)

;; Use UTF-8 by default
(prefer-coding-system        'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Trailing whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Left fringe is 4 pixels; right fringe is gone
(fringe-mode '(4 . 0))

;; org-mode
(use-package org
  :config
  (progn
    (setq-default org-display-custom-times t)
    (setq org-time-stamp-custom-formats
          '("<%Y-%m-%d>" . "<%Y/%m/%d %a %H:%M>"))))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")
