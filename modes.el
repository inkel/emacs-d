;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (quote ("\\` " "\\*\\(Messages\\|scratch\\|Help\\|Completions\\)\\*" "\\*magit-" "\\*ag "))
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
(require 'magit)
(defun inkel/magit-log-edit-mode-hook ()
  (setq fill-column 72)
  (flyspell-mode t)
  (turn-on-auto-fill))
(add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)

;; Ruby and related modes
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"      . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"     . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.god$"     . enh-ruby-mode))

(require 'mote-mode)
(add-to-list 'auto-mode-alist '("\\.mote$" . html-mode))
(add-hook 'html-mode-hook 'mote-mode)

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
