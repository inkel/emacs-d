;; Disable startup screen
(setq startup-screen-inhibit-startup-screen t)
(setq inhibit-startup-screen t)

;; Delete selected text if overwritten
(delete-selection-mode t)

;; Use y/n instead of yes/no for questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Incremental minibuffer completion
(icomplete-mode t)

;; Adds a new line at the last line
(setq require-final-newline t)

;; Tags filename
(setq tags-file-name "TAGS")

;; Use system clipboard for copy/paste
(setq x-select-enable-clipboard t)

;; Reload files when changed
(global-auto-revert-mode 1)

;; Save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode t)

;; Save cursor position within files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.saveplace")

;; Disable backups
(setq backup-inhibited t)

;; Tab either indents or complete
(setq tab-always-indent 'complete)

(provide 'settings)
