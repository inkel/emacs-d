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
  ;; Start Emacs in full screen mode
  (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

  (setq default-directory "~/")

  ;; Custom variables && configuration
  (setq custom-file "~/.emacs.d/custom.el")
  (if (file-exists-p custom-file)
      (load custom-file))

  (let* ((computer-name (string-trim-right (system-name) ".local"))
         (computer-custom-file (concat user-emacs-directory computer-name ".el")))
    (when (file-exists-p computer-custom-file)
      (load computer-custom-file)))

  ;; Package management
  (customize-set-variable 'package-archives
                          '(("gnu"   . "https://elpa.gnu.org/packages/")
                            ("org"   . "https://orgmode.org/elpa/")
                            ("melpa" . "https://melpa.org/packages/")))

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
  ;;; Completion - https://company-mode.github.io/
  (use-package company
    :hook (after-init . global-company-mode))
  ;;; Yasnippet - http://joaotavora.github.io/yasnippet/
  (use-package yasnippet
    :config (yas-global-mode))

  ;;; Paste where cursor is
  (customize-set-variable 'mouse-yank-at-point t)

  ;;; Show line numbers
  (use-package display-line-numbers
    :defer nil
    :ensure nil
    :config (global-display-line-numbers-mode))

  ;;; Matching parenthesis
  (show-paren-mode)

  ;;; Fuck tabs!
  (customize-set-variable 'indent-tabs-mode nil)

  ;;; Those pesky backups files…
  (customize-set-variable 'backup-directory-alist
                          `(("." . ,(concat user-emacs-directory "backups"))))

  ;; Adds a new line at the last line
  (setq require-final-newline t)

  ;; Analyze script hash-bang and mark it as executable if possible on
  ;; first saven
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;;; Sound off
  (setq ring-bell-function 'ignore)
  (setq visible-bell t)

  ;;; Delete highlighted region
  (delete-selection-mode t)

  ;;; Use y/n instead of yes/no for questions
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;;; Use system clipboard for copy/paste
  (setq select-enable-clipboard t)

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
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;;; Fix $PATH in macOS
  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :config (exec-path-from-shell-initialize))

  ;; Key bindings
  (require 'bind-key)

  (when (string= "darwin" system-type)
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

  ;;; Use crux for some useful things, like C-a moving to the first
  ;;; character of the line instead of the beggining of the line!.
  ;;; https://github.com/bbatsov/crux
  ;;; http://pragmaticemacs.com/emacs/open-files-with-the-system-default-application/
  ;;; http://pragmaticemacs.com/emacs/move-to-the-beginning-of-a-line-the-smart-way/
  (use-package crux
    :bind (("C-c o" . crux-open-with)
           ("C-a" . crux-move-beginning-of-line)))

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
      (set-face-attribute 'default nil :family "Go Mono" :height 160))

  ;;; https://coding-fonts.css-tricks.com/fonts/source-code-pro/
  (condition-case nil
      (set-face-attribute 'default nil :family "Source Code Pro" :height 140))

  ;;; https://coding-fonts.css-tricks.com/fonts/hack/
  (condition-case nil
      (set-face-attribute 'default nil :family "Hack" :height 140))

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

  ;;; ibuffer
  (use-package ibuffer
    :bind (("C-x C-b" . ibuffer))
    :config
    (setq ibuffer-default-sorting-mode 'filename/process))

  ;; Magit - Enough reason to use Emacs
  (use-package magit
    :bind (("C-x g" . magit-status))
    :config
    ;; Speeding up magit-status
    ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    (defun inkel/magit-log-edit-mode-hook ()
      (setq fill-column 72)
      (flyspell-mode t)
      (turn-on-auto-fill))
    (add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)
    (setq vc-handled-backends (delq 'Git vc-handled-backends))
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows)))

  ;; ibuffer
  (use-package ibuffer
    :bind ("C-x C-b" . ibuffer))

  ;; Incremental narrowing
  (use-package selectrum
    :config (selectrum-mode +1))

  ;; the silver searcher
  (use-package ag
    :config (setq ag-highlight-search t
                  ag-reuse-window t
                  ag-reuse-buffers t))

  ;; undo-tree
  ;; http://www.dr-qubit.org/undo-tree.html
  (use-package undo-tree
    :config (global-undo-tree-mode))

  ;; ace-window - navigate windows easily
  (use-package ace-window
    :bind ("M-o" . ace-window))

  ;; Reload Emacs config
  (defun inkel/reload-emacs-configuration ()
    (interactive)
    (let ((before-time (current-time))
          (gcs gcs-done))
      (load-file "~/.emacs.d/init.el")
      (message "Emacs ready in %.2f seconds with %d garbage collections."
               (float-time (time-subtract (current-time) before-time))
               (- gcs-done gcs))))
  (global-set-key (kbd "M-r") 'inkel/reload-emacs-configuration)

  ;; Grafana
  ;;; Jsonnet
  (use-package jsonnet-mode
    :config (setq jsonnet-indent-level 2))

  ;;(use-package terraform-mode)

  (use-package yaml-mode)

  (use-package dockerfile-mode)

  ;; General programming
  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :init (setq lsp-keymap-prefix "s-l")
    :hook ((before-save . lsp-format-buffer)
           (before-save . lsp-organize-imports)
           (go-mode . lsp-deferred)))

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  ;; Go programming
  (setenv "GOPATH" "/Users/inkel/dev/go")
  (setq lsp-gopls-staticcheck t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-gopls-complete-unimported t)

  (use-package go-mode
    :hook (go-mode . lsp))

  ;; Markdown - https://jblevins.org/projects/markdown-mode/
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  ;; Org
  (use-package org
    :pin org
    :config
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-switchb)
    (global-set-key "\C-cl" 'org-store-link)

    (use-package ox-gfm
      :ensure t
      :after org)

    (require 'ob)

    (require 'ob-shell)

    (require 'org-tempo)

    :init
    (setq org-babel-no-eval-on-ctrl-c-ctrl-c nil
          org-confirm-babel-evaluate nil)

    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t))))

  ;; Dired - http://xenodium.com/showhide-emacs-dired-details-in-style/
  (use-package dired
    :ensure nil
    :hook (dired-mode . dired-hide-details-mode)
    :config
    ;; Colourful columns.
    (use-package diredfl
      :config
      (diredfl-global-mode 1))
    (use-package dired-git-info
      :ensure t
      :bind (:map dired-mode-map
                  (")" . dired-git-info-mode))))

  ;; Lisp & Clojure
  (use-package cider
    :hook clojure-mode-hook
    :config
    ;; (setenv "JAVA_HOME" "/usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home")
    (exec-path-from-shell-copy-env "JAVA_HOME")
    (setq cider-repl-pop-to-buffer-on-connect 'display-only)
    (setq cider-repl-display-in-current-window nil))

  ;; Server
  (use-package server
    :config (unless (server-running-p)
              (server-start)))

   ;;; Trailing whitespace is bad and you should feel bad
  (customize-set-variable 'show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Just a message for me, and a placeholder to add stuff at the end
  ;; without having to change too many lines.
  (message "Ready to rock"))
;; End prevent of special filename parsing

;; Set GC threshold to 1GB
(setq gc-cons-threshold (* 1000 1000))
