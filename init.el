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
(require 'packages)
(require 'modes)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq citrusbyte-file "~/.emacs.d/citrusbyte.el")
(if (file-exists-p citrusbyte-file)
    (load citrusbyte-file))

(unless (server-running-p)
  (server-start))
