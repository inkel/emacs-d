;;; init.el --- My Emacs config

;;; Commentary:
;; 2023 version of my Emacs configuration

;;; Code:

;; Raise GC threshold to maximum available
(setq gc-cons-percentage 0.4
      gc-cons-threshold (* 128 1024 1024))

;; Use a hook so the message doesn't get clobbered by other messages.
(defun inkel/report-init-time-and-gc ()
  "Report initialization duration and garbage collections."
  (message "Greetings, professor Falken. Emacs ready in %.2fs with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'inkel/report-init-time-and-gc)

(add-hook 'after-init-hook #'garbage-collect t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(defconst user-data-directory (emacs-path "data"))

(defun user-data (dir)
  "Expands DIR filename within `user-data-directory'."
  (expand-file-name dir user-data-directory))

;; Package management
(eval-and-compile
  ;; do not load installed packages
  (setq package-enable-at-startup nil))

(require 'package)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(defvar use-package-enable-imenu-support t)
(require 'use-package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/"))
      use-package-always-ensure t
      use-package-always-defer t
      use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      debug-on-error init-file-debug)

;; Disable warnings about obsolete functions when compiling
(eval-when-compile
  (dolist (sym '(cl-flet lisp-complete-symbol))
    (setplist sym (use-package-plist-delete
                   (symbol-plist sym) 'byte-obsolete-info))))

;; Do not handle special filenames in any way during startup
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))

;; Start Emacs in full screen mode
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
(advice-add 'restart-emacs :before 'toggle-frame-fullscreen)

;; pseudo-package representing Emacs
(use-package emacs
  :bind (("C-z" . nil) ; do not send to background/minimize by mistake
         ("s-q" . nil) ; do not close Emacs on Mac by mistake
         ("s-p" . nil) ; do not print on Mac by mistake

         ("H-`" . imenu)

         ("C-x C-p" . find-file-at-point))

  :init
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (setq x-stretch-cursor t)

  :custom
  ;; Mac stuff
  (ns-alternate-modifier 'meta "Option is Meta")
  (ns-right-alternate-modified 'alt)
  (ns-command-modifier 'super "Command is Super")
  (ns-right-command-modifier 'super)
  (ns-function-modifier 'hyper "Function is Hyper")

  ;; Disable startup screen
  (startup-screen-inhibit-startup-screen t)
  (inhibit-startup-screen t)

  ;; History
  (history-delete-duplicates t)
  (history-length 100)

  ;; Visual
  (visible-bell t)
  (ring-bell-function 'ignore)
  (show-trailing-whitespace t)

  ;; Misc
  (load-prefer-newer t)
  (default-directory "~/")

  (select-enable-clipboard t) ; use system clipboard for copy/paste
  (mouse-yank-at-point t) ; paste where cursor is

  (custom-file "~/.emacs.d/custom.el")

  (require-final-newline t) ; adds a new line at the last line

  :config
  (delete-selection-mode t) ; delete highlighted region
  (defalias 'yes-or-no-p 'y-or-n-p) ; use y/n instead of yes/no for questions

  ;; Use UTF-8 by default
  (prefer-coding-system        'utf-8)
  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)

  (global-auto-revert-mode 1) ; reload files when changed

  (add-hook 'before-save-hook #'delete-trailing-whitespace) ; trailing whitespace is bad and you should feel bad

  ;; Custom variables && configuration
  (if (file-exists-p custom-file)
      (load custom-file))

  (let* ((computer-name (string-trim-right (system-name) ".local"))
         (computer-custom-file (concat user-emacs-directory computer-name ".el")))
    (when (file-exists-p computer-custom-file)
      (load computer-custom-file))))

(condition-case nil
    (set-face-attribute 'default nil :family "Go Mono" :height 140))

;; Visual settings
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode -1))


;; No tabs, no
(customize-set-variable 'indent-tabs-mode nil)

;; Indentation
(setq standard-indent 2)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; https://karthinks.com/software/a-tab-bar-menu-in-emacs/
(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)

;; Analyze script hash-bang and mark it as executable if possible on
;; first saven
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Disable backups
(setq backup-inhibited t)

;; Misc settings
(setq query-replace-highlight t	 ; Highlight matches during query replacement
      search-highlight t           ; Highlight current search match
      column-number-mode t         ; Column number on status bar
      size-indication-mode t       ; Show buffer size
      blink-cursor-mode t          ; Blink the cursor
      frame-title-format "%b (%f)" ; Frame title format
      )

;; Show line numbers
(use-package display-line-numbers
  :defer nil
  :ensure nil
  :config (global-display-line-numbers-mode))

;; ace-window - navigate windows easily
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Make it easy to identify each open file
(use-package uniquify
  :defer 1
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t))

;; ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-default-sorting-mode 'filename/process))

;; Completion - https://company-mode.github.io/
;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :custom (tab-always-indent . 'complete))

;; corfu for completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :commands
  (global-corfu-mode corfu-history-mode)
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Glorious package to let you know what binding are available
(use-package which-key
  :defer nil
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; Use crux for some useful things, like C-a moving to the first
;; character of the line instead of the beggining of the line!.
;; https://github.com/bbatsov/crux
;; http://pragmaticemacs.com/emacs/open-files-with-the-system-default-application/
;; http://pragmaticemacs.com/emacs/move-to-the-beginning-of-a-line-the-smart-way/
(use-package crux
  :bind (("C-c M-o" . crux-open-with)
	     ("C-k" . crux-kill-and-join-forward)
	     ("C-x 4 t" . crux-transpose-windows)
	     ("C-x D" . crux-delete-file-and-buffer)
	     ("C-x M-r" . crux-rename-file-and-buffer)
	     ("s-k" . crux-kill-whole-line)
         ("C-a" . crux-move-beginning-of-line)))

(defvar treesit-language-source-alist '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
	                                    (go "https://github.com/tree-sitter/tree-sitter-go")
	                                    (gomod "https://github.com/camdencheek/tree-sitter-gomod")))

(use-package tree-sitter
  :config
  (dolist (grammar treesit-language-source-alist)
    (treesit-install-language-grammar (car grammar))))

(use-package project)

(defun project-find-go-module (dir)
  "Find DIR first parent directory defining a go.mod file."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return root directory of the current Go PROJECT."
  (cdr project))

(setq-default eglot-workspace-configuration
              '((:gopls . ((staticcheck . t)
                           (matcher . "CaseSensitive")))))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-ts-mode
  :custom (go-ts-mode-indent-offset 8)
  :mode "\\.go$")

(add-to-list 'major-mode-remap-alist
             '(go-mode . go-ts-mode))

(defun inkel/eglot-organize-imports-on-save ()
  "Hook for organizing Go imports when saving buffer."
  (call-interactively 'eglot-code-action-organize-imports))

(defun eglot-format-buffer-on-save ()
  "Hook for Go projects when saving buffer."
  (add-hook 'before-save-hook #'inkel/eglot-organize-imports-on-save)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package eglot
  :hook ((go-ts-mode . eglot-format-buffer-on-save)
         (go-ts-mode . eglot-ensure))

  :commands (eglot-format-buffer
             eglot-code-action-organize-imports)

  :bind (:map eglot-mode-map
	          ("C-c C-d" . eldoc)
	          ("C-c C-r" . eglot-rename)
	          ("C-c C-a" . eglot-code-actions)
              ("s-i" . imenu)))

;; ibuffer
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
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

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

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :bind (;; Better yank?
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)))

(use-package consult-eglot :after consult)

(use-package marginalia
  :bind (:map minibuffer-mode-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package smartscan
  :ensure t
  :commands smartscan-mode
  :bind (:map prog-mode-map
              ("M-n" . smartscan-symbol-go-forward)
              ("M-p" . smartscan-symbol-go-backward))
  :init (add-hook 'prog-mode-hook #'smartscan-mode)
  :diminish)

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :commands (flymake-goto-next-error
             flymake-goto-prev-error)
  :bind (:map flymake-mode-map
              ("s-n" . 'flymake-goto-next-error)
              ("s-p" . 'flymake-goto-prev-error)))

(defun inkel/go-file-imports (filename)
  (cl-flet* ((stdlib-p (pkg) (not (string-match-p "\\." pkg)))
             (sort-packages (pkgs) (sort pkgs #'string-lessp)))
    (let ((imports (process-lines "go" "list" "-f" "{{range .Imports}}{{println .}}{{end}}" filename)))
      (seq-concatenate 'list
                       (sort-packages (seq-filter #'stdlib-p imports))
                       (sort-packages (seq-remove #'stdlib-p imports))))))

(defun inkel/go-package-docs (&optional package)
  "Open up Go package PACKAGE documentation in pkg.go.dev."
  (interactive (list (let ((packages (inkel/go-file-imports buffer-file-name))
                           (vertico-sort-function nil))
                       (completing-read "Browse pkg.go.dev documentation for: " packages))))
  (browse-url (concat "https://pkg.go.dev/" package)))

(use-package go-ts-mode
  :bind (:map go-ts-mode-map
	          ("C-c M-p" . inkel/go-package-docs)))

(use-package terraform-mode
  :hook ((terraform-mode . terraform-format-on-save-mode))
  :commands (company-terraform-init)
  :config (use-package company-terraform
            :config (company-terraform-init)))

(use-package yaml-mode)

(use-package dockerfile-mode)

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

  ;; (require 'ob-promql "~/.emacs.d/ob-promql.el")

  :init
  (setq org-babel-no-eval-on-ctrl-c-ctrl-c nil
        org-confirm-babel-evaluate nil)
  ;;(setq org-src-block-faces '(("shell" (:foreground "#eeeeee"))))
  (setq org-adapt-indentation nil)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package savehist
  :unless noninteractive
  :custom
  (savehist-additional-variables
   '(file-name-history
     kmacro-ring
     compile-history
     compile-command))
  (savehist-autosave-interval 60)
  (savehist-file (user-data "history"))
  (savehist-ignored-variables
   '(load-history
     flyspell-auto-correct-ring
     org-roam-node-history
     magit-revision-history
     org-read-date-history
     query-replace-history
     yes-or-no-p-history
     kill-ring))
  (savehist-mode t)
  :config
  (savehist-mode 1))

(use-package saveplace
  :unless noninteractive
  :custom
  (save-place-file (user-data "places"))
  :config
  (save-place-mode 1))

;; Server
(use-package server
  :commands (server-running-p)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :defines (yas-snippet-dirs)
  :commands (yas-reload-all)
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

;; ;; Prevent special filename parsing
;; (let ((file-name-handler-alist nil))

;;   ;; Misc settings

;;   ;;; Yasnippet - http://joaotavora.github.io/yasnippet/
;;   (use-package yasnippet
;;     :config (yas-global-mode))


;;   ;;; Matching parenthesis
;;   (show-paren-mode)

;;   ;;; Those pesky backups files…
;;   (customize-set-variable 'backup-directory-alist
;;                           `(("." . ,(concat user-emacs-directory "backups"))))


;;   ;;; Fix $PATH in macOS
;;   (use-package exec-path-from-shell
;;     :if (memq window-system '(mac ns))
;;     :config (exec-path-from-shell-initialize))

;;   ;; Key bindings
;;   (require 'bind-key)

;; I love this package, even when I only use one function
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-S->" . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<" . mc/unmark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Install fonts for modeline - https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; Doom modeline - https://github.com/seagle0128/doom-modeline
;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode))

;; Doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)

  :commands (doom-themes-visual-bell-config
             doom-themes-org-config)

  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(load-theme 'doom-solarized-light t)

(use-package shell-pop
  :bind (("C-x t" . shell-pop))
  :custom
  (shell-pop-universal-key "C-x t")
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))))

;;   ;;; Fonts
;;   ;;; https://blog.golang.org/go-fonts
;;   ;;; https://coding-fonts.css-tricks.com/fonts/source-code-pro/
;;   ;;; https://coding-fonts.css-tricks.com/fonts/hack/
;;   (defun inkel/set-font (family)
;;     (condition-case nil
;;         (set-face-attribute 'default nil :family family :height 140)))

;;   (defun inkel/set-font-go-mono () (interactive) (inkel/set-font "Go Mono"))
;;   (defun inkel/set-font-source-code-pro () (interactive) (inkel/set-font "Source Code Pro"))
;;   (defun inkel/set-font-hack () (interactive) (inkel/set-font "Hack"))

;;   (inkel/set-font-go-mono)

;;   (use-package forge
;;     :after magit)

;;   ;; the silver searcher
;;   (use-package ag
;;     :config (setq ag-highlight-search t
;;                   ag-reuse-window t
;;                   ag-reuse-buffers t))

;;   ;; undo-tree
;;   ;; http://www.dr-qubit.org/undo-tree.html
;;   (use-package undo-tree
;;     :config
;;     (global-undo-tree-mode)
;;     (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;;   ;; Reload Emacs config
;;   (defun inkel/reload-emacs-configuration ()
;;     (interactive)
;;     (let ((before-time (current-time))
;;           (gcs gcs-done))
;;       (load-file "~/.emacs.d/init.el")
;;       (message "Emacs ready in %.2f seconds with %d garbage collections."
;;                (float-time (time-subtract (current-time) before-time))
;;                (- gcs-done gcs))))
;;   (global-set-key (kbd "M-r") 'inkel/reload-emacs-configuration)

;;   ;; Grafana
;;   (defun inkel/on-call ()
;;     (interactive)
;;     (find-file "~/dev/grafana/org/on-call.org"))

;;   (defun inkel/find-file-deployment_tools (filename)
;;     (interactive (list (read-file-name "Find file: " nil "~/dev/grafana/repos/deployment_tools/" (confirm-nonexistent-file-or-buffer))))
;;     (switch-to-buffer (find-file-noselect filename)))
;;   (global-set-key (kbd "C-c C-x C-f") 'inkel/find-file-deployment_tools)

;;   ;; avy - https://www.youtube.com/watch?v=SZ9ciomvgNo
;;   ;; https://irreal.org/blog/?p=11507
;;   (use-package avy
;;   :ensure t
;;   :diminish avy-mode
;;   :bind (("s-a"     . avy-goto-word-1)
;;          ("s-s"     . avy-goto-char-timer)
;;          ("M-g M-g" . avy-goto-line)
;;          ("M-g g"   . avy-goto-line)
;;          ("s-A"     . avy-goto-char))
;;   :config (setq avy-all-windows nil))

;;   ;;; Jsonnet
;;   (use-package jsonnet-mode
;;     :config (setq jsonnet-indent-level 2))

;;   (use-package jq-mode
;;     :mode (("\\.jq$" . jq-mode))
;;     :bind (:map jsonnet-mode-map
;;                 ("C-c Cj" . jq-interactively)))

;;   (use-package jsonian
;;     :after so-long
;;     :custom (jsonian-no-so-long-mode))

;;   ;; HCL
;;   (defun inkel/hclfmt-region ()
;;     (interactive)
;;     (shell-command-on-region (region-beginning) (region-end)
;;                              "hclfmt"
;;                              nil
;;                              t))

;;   (defun inkel/hclfmt-buffer ()
;;     (interactive)
;;     (shell-command-on-region (point-min) (point-max)
;;                              "hclfmt"
;;                              nil
;;                              t))


;;   ;; General programming

;;   ;; Go programming
;;   (setenv "GOPATH" "/Users/inkel/dev/go")
;;   (setq lsp-gopls-staticcheck t)
;;   (setq lsp-eldoc-render-all t)
;;   (setq lsp-gopls-complete-unimported t)

;;   (use-package go-mode
;;     :bind (:map go-mode-map
;;                 ("s-l d" . inkel/go-package-docs))
;;     :hook (go-mode . lsp))

;;   ;; Rust yuck
;;   (defun inkel/rust-hook ()
;;     (setq indent-tabs-mode nil
;;           rust-format-on-save t))
;;   (use-package rust-mode
;;     :config
;;     (add-hook 'rust-mode-hook 'inkel/rust-hook)
;;     (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;;   ;; Markdown - https://jblevins.org/projects/markdown-mode/
;;   (use-package markdown-mode
;;     :ensure t
;;     :commands (markdown-mode gfm-mode)
;;     :mode (("README\\.md\\'" . gfm-mode)
;;            ("\\.md\\'" . markdown-mode)
;;            ("\\.markdown\\'" . markdown-mode))
;;     :init (setq markdown-command "multimarkdown"))



;;   (setq org-directory "~/dev/grafana/org")
;;   (setq org-default-notes-file (concat org-directory "/notes.org"))
;;   (define-key global-map "\C-cc" 'org-capture)

;;   (setq org-capture-templates
;;         '(("o" "On-Call" item (file+datetree "on-call.org") "")))

;; Create non-existing directory automatically
;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
(defun inkel/auto-create-missing-directory ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))
(add-to-list 'find-file-not-found-functions #'inkel/auto-create-missing-directory)

;; Dired - http://xenodium.com/showhide-emacs-dired-details-in-style/
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map global-map
              ("C-x C-d" . dired-jump))
  :config
  ;; Reuse buffers - https://www.manueluberti.eu//emacs/2021/07/14/dired/
  (setq dired-kill-when-opening-new-dired-buffer t)
  (use-package dired-single)
  ;; Colourful columns.
  (use-package diredfl
    :commands (diredfl-global-mode)
    :config
    (diredfl-global-mode 1))
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))
  (use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode))))

;;   ;; Lisp & Clojure
;;   (use-package cider
;;     :hook clojure-mode-hook
;;     :config
;;     ;; (setenv "JAVA_HOME" "/usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home")
;;     (exec-path-from-shell-copy-env "JAVA_HOME")
;;     (setq cider-repl-pop-to-buffer-on-connect 'display-only)
;;     (setq cider-repl-display-in-current-window nil))

;;   ;; ;; Email - https://www.youtube.com/watch?v=yZRyEhi4y44
;;   ;; ;;; https://github.com/daviwil/emacs-from-scratch/blob/629aec3dbdffe99e2c361ffd10bd6727555a3bd3/show-notes/Emacs-Mail-01.org
;;   ;; (use-package mu4e
;;   ;;   :ensure nil
;;   ;;   :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
;;   ;;   :config

;;   ;;   ;; This is set to 't' to avoid mail syncing issues when using mbsync
;;   ;;   (setq mu4e-change-filenames-when-moving t)

;;   ;;   ;; Refresh mail using isync every 10 minutes
;;   ;;   (setq mu4e-update-interval (* 10 60))
;;   ;;   (setq mu4e-get-mail-command "mbsync -a")
;;   ;;   (setq mu4e-maildir "~/Mail")

;;   ;;   (use-package mu4e-alert
;;   ;;     :config
;;   ;;     (mu4e-alert-enable-mode-line-display)
;;   ;;     (setq doom-modeline-mu4e t))

;;   ;;   (setq mu4e-drafts-folder "/[Gmail]/Drafts")
;;   ;;   (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
;;   ;;   (setq mu4e-refile-folder "/[Gmail]/All Mail")
;;   ;;   (setq mu4e-trash-folder  "/[Gmail]/Trash")

;;   ;;   (setq mu4e-maildir-shortcuts
;;   ;;         '((:maildir "/Inbox"    :key ?i)
;;   ;;           (:maildir "/[Gmail]/Sent Mail" :key ?s)
;;   ;;           (:maildir "/[Gmail]/Trash"     :key ?t)
;;   ;;           (:maildir "/[Gmail]/Drafts"    :key ?d)
;;   ;;           (:maildir "/[Gmail]/All Mail"  :key ?a))))

;;   ;; Just a message for me, and a placeholder to add stuff at the end
;;   ;; without having to change too many lines.
;;   (message "Ready to rock"))
;; ;; End prevent of special filename parsing

;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)
;; (put 'magit-edit-line-commit 'disabled nil)

(provide 'init)
;;; init.el ends here
