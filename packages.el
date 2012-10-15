(defun use-package (package-name)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(use-package 'multiple-cursors)
(use-package 'mote-mode)

(provide 'packages)
