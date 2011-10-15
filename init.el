(require 'cl)

;; Custom settings goes here
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; (setq byte-compile-warnings t)
;; (load-file "~/.emacs.d/site-lisp/byte-code-cache.el")

;;; Editor options
(savehist-mode t)
(delete-selection-mode t)
(setq startup-screen-inhibit-startup-screen t)
(setq inhibit-startup-screen t)
(setq yes-or-no-p 'y-or-n-p)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq column-number-mode t)
(setq size-indication-mode t)
(icomplete-mode t)
(setq require-final-newline t)
'(recentf-mode)
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq tags-file-name "TAGS")
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(require 'color-theme)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/vendors/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)
;; Fix the annoying X issue with Emacs and X clipboard
(setq x-select-enable-clipboard t)

;;; Visual configurations
(progn
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(global-linum-mode t)
'(transient-mark-mode t)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq column-number-mode t)
(set-foreground-color "#dbdbdb")
(set-background-color "#000000")
(set-cursor-color "#ff0000")

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)

;;; Backups
(setq make-backup-files nil)
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

;; Kill buffers
(defun kill-others ()
  "Kill all buffers but this"
  (interactive)
  (mapcar (lambda (buffer)
            (if (not
                  (eq buffer (current-buffer)))
                (kill-buffer buffer)))
            (buffer-list))
  (delete-other-windows))

(defun kill-all-buffers ()
  "Kill all opened buffers but, leaving *scratch* only."
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;;; Key bindings
(global-set-key (kbd "<f12>") 'ido-mode)
(global-set-key (kbd "C-<f4>") 'kill-others)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "<f9>") 'comment-region)
(global-set-key (kbd "<M-f9>") 'uncomment-region)
(global-set-key (kbd "C-z") 'nil)
(global-set-key (kbd "C-x C-z") 'nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-c l") 'org-todo)

(global-set-key [s-left]  'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up]    'windmove-up)
(global-set-key [s-down]  'windmove-down)

;;; Auto-complete
;; (add-to-list 'load-path "~/.emacs.d/vendors/auto-complete/")
;; (require 'auto-complete-config)
;; (global-auto-complete-mode t)
;; (setq ac-auto-start t)
;; (setq ac-dwim 3)
;; (setq ac-override-local-map nil)
(setq tab-always-indent 'complete)

;;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Ruby & Ruby on Rails
(add-to-list 'load-path "~/.emacs.d/vendors/rinari/")
(require 'rinari)
(setq rinari-tags-file-name "TAGS")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.god$" . ruby-mode))

(load-file "~/.emacs.d/site-lisp/ruby-block.el")

(add-to-list 'load-path  "~/.emacs.d/vendors/ruby-electric")

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max)))))
            (require 'ruby-electric)
            (ruby-electric-mode t)
            (require 'ruby-block)
            (ruby-block-mode t)
            (set (make-local-variable 'ruby-block-mode-toggle) t)
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)))

(autoload 'yaml-mode "~/.emacs.d/vendors/yaml-mode/yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "RET" 'newline-and-indent)))

(load-file "~/.emacs.d/vendors/haml-mode/haml-mode.el")
(require 'haml-mode) ;; Required by sass-mode
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(autoload 'sass-mode "~/.emacs.d/vendors/sass-mode/sass-mode")
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'feature-mode "~/.emacs.d/vendors/cucumber.el/feature-mode")
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; CoffeeScript
(autoload 'coffee-mode "~/.emacs.d/vendors/coffee-mode/coffee-mode")
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Code snippets
(add-to-list 'load-path "~/.emacs.d/vendors/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendors/yasnippet/snippets")

;; TextMate minor mode
;; Absolutely amazing
(add-to-list 'load-path "~/.emacs.d/vendors/textmate.el")
(require 'textmate)
(textmate-mode)

;; Magit
(add-to-list 'load-path "~/.emacs.d/vendors/magit")
(require 'magit)

;; jade-mode
(add-to-list 'load-path "~/.emacs.d/vendors/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . sws-mode))

;; espresso-mode
;;(load-file "~/.emacs.d/site-lisp/espresso.el")
(autoload 'espresso-mode "~/.emacs.d/site-lisp/espresso")
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(setq espresso-indent-level 2)

;; Disable annoying ido-mode feature that will try open an existing
;; file of the same name
(setq ido-use-url-at-point nil
      ido-everywhere t
      ido-ignore-buffers (quote ("\\` " "\\*\\w+\\*"))
      ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\.git")))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)

(setq frame-title-format "%b (%f)")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
