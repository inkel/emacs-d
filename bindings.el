;; Move through windows using meta-<arrows>
(if (string= "darwin" system-type)
    (windmove-default-keybindings 'meta))

;; Do not close Emacs on Command-q
(global-set-key (kbd "s-q") nil)

(global-set-key (kbd "<f12>") 'ido-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x Cb") 'ibuffer)

;; Do not send to background/minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Textmate
(global-set-key (kbd "C-x t") 'textmate-clear-cache)

(provide 'bindings)
