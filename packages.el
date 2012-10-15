(defun use-package (package-name)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(provide 'packages)
