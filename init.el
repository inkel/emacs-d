(add-to-list 'load-path "~/.emacs.d")

(require 'settings)
(require 'visual)
(require 'bindings)
(require 'editing)
(require 'defuns)
(require 'modes)

;; Custom settings
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
