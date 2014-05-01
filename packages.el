(defun use-package (package-name)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(use-package 'smex)
(use-package 'ag)
(use-package 'multiple-cursors)
(use-package 'mote-mode)
(use-package 'exec-path-from-shell)
(use-package 'enh-ruby-mode)

(provide 'packages)
