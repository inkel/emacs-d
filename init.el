(add-to-list 'load-path "~/.emacs.d")

;; Custom settings
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(require 'defuns)
(require 'settings)
(require 'visual)
(require 'bindings)
(require 'editing)
(require 'modes)
(require 'packages)

(unless (server-running-p)
  (server-start))
