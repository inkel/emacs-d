;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (quote ("\\` " "\\*\\(Messages\\|scratch\\|Help\\|Completions\\)\\*" "\\*magit-"))
      ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\.git")))

;; Smex
(require 'smex)
(smex-initialize)

;; Analyze script hash-bang and mark it as executable if possible on
;; first save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; JavaScript
(defun inkel/javascript-mode-hook ()
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'inkel/javascript-mode-hook)

;; Magit
(vendor 'magit)
(defun inkel/magit-log-edit-mode-hook ()
  (setq fill-column 72)
  (flyspell-mode t)
  (turn-on-auto-fill))
(add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)

;; Textmate mode
(vendor 'textmate)
(textmate-mode t)

;; Ruby and related modes
(vendor 'ruby-electric)

(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.god$"     . ruby-mode))

(add-hook 'ruby-mode-hook (lambda ()
                            (ruby-electric-mode t)))

(autoload 'yaml-mode "~/.emacs.d/vendor/yaml-mode/yaml-mode.el")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Haml & Sass
(vendor 'haml-mode)
(autoload 'sass-mode "~/.emacs.d/vendor/sass-mode/sass-mode.el")
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Markdown
(autoload 'markdown-mode "vendor/markdown-mode/markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'mote-mode)
(add-to-list 'auto-mode-alist '("\\.mote$" . html-mode))
(add-hook 'html-mode-hook 'mote-mode)

(autoload 'nginx-mode "~/.emacs.d/vendor/nginx-mode.el")
(add-to-list 'auto-mode-alist '("\\.nginx$"     . nginx-mode))

;; Magit
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores previous window configuration and kills magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'modes)
