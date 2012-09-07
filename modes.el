;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (quote ("\\` " "\\*\\(Messages\\|scratch\\|Help\\|Completions\\)\\*" "\\*magit-"))
      ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\.git")))

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

;; Textmate mode
(vendor 'textmate)
(textmate-mode t)

;; Ruby and related modes
(vendor 'rvm)
(rvm-autodetect-ruby)

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

(vendor 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Haml & Sass
(vendor 'haml-mode)
(vendor 'sass-mode)

;; Markdown
(autoload 'markdown-mode "vendor/markdown-mode/markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(provide 'modes)
