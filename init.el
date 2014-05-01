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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(unless (server-running-p)
  (server-start))
