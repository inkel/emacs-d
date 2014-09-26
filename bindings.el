;; Move through windows using meta-<arrows>
(when (string= "darwin" system-type)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  (global-set-key (kbd "s-<left>")  'windmove-left)
  (global-set-key (kbd "s-<up>")    'windmove-up)
  (global-set-key (kbd "s-<down>")  'windmove-down))

;; Do not close Emacs on Command-q
(global-set-key (kbd "s-q") nil)

;; Do not display print dialog
(global-set-key (kbd "s-p") nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f12>") 'ido-mode)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Do not send to background/minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Resize windows
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-s-<down>")  'enlarge-window)
(global-set-key (kbd "C-s-<up>")    'shrink-window)

;; Multiple cursors
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->")     'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; Company mode
(global-set-key (kbd "C-<tab>") 'company-complete)

(provide 'bindings)
