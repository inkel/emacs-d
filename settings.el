;; Disable annoying message on some commands
(setq disabled-command-function nil)

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

;; Disable backups
(setq backup-inhibited t)

;; Tab either indents or complete
(setq tab-always-indent 'complete)

;; Add brew binaries to PATH
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell")

;; Highlight search results from ag
(setq ag-highlight-search t)

(provide 'settings)
