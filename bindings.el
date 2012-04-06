;; Move through windows using meta-<arrows>
(if (string= "darwin" system-type)
    (windmove-default-keybindings 'meta))

(global-set-key (kbd "<f12>") 'ido-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x Cb") 'ibuffer)

;; Do not send to background/minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

(provide 'bindings)
