(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun my/use-package (package-name)
  (unless (package-installed-p package-name)
    (package-install package-name)))

(my/use-package 'ag)
(my/use-package 'multiple-cursors)
(my/use-package 'mote-mode)
(my/use-package 'enh-ruby-mode)
(my/use-package 'markdown-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind
  (("C-c C-r" . go-remove-unused-imports)
   ("C-c C-g" . go-goto-imports)
   ("C-c C-f" . gofmt)
   ("C-c C-k" . godoc)))

(use-package company
  :ensure t
  :defer t
  :bind (("C-<tab>" . company-complete))
  :config (global-company-mode))

(use-package smex
  :ensure t
  :config (smex-initialize))

(use-package magit
  :ensure t
  :config (progn
            (add-hook 'magit-log-edit-mode-hook (lambda ()
                                                  (setq fill-column 72)
                                                  (flyspell-mode t)
                                                  (turn-on-auto-fill)))
            (defadvice magit-status (around magit-fullscreen activate)
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))))

(provide 'packages)
