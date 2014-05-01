(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun use-package (package-name)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(use-package 'magit)
(use-package 'smex)
(use-package 'ag)
(use-package 'multiple-cursors)
(use-package 'mote-mode)
(use-package 'exec-path-from-shell)
(use-package 'enh-ruby-mode)
(use-package 'markdown-mode)

(provide 'packages)
