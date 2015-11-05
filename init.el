(add-to-list 'load-path "~/.emacs.d")

;; Custom settings
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(require 'packages)
(require 'settings)
(require 'visual)
(require 'modes)
(require 'editing)
(require 'bindings)

(setq scroll-margin 5
      scroll-preserve-screen-position 1)

;; org-mode
(setq org-agenda-files '("~/dev/citrusbyte/m2x/m2x.org"))
(global-set-key (kbd "C-x a") 'org-agenda)

(unless (server-running-p)
  (server-start))
