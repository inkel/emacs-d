(require 'cl)

;;; Editor options
(savehist-mode t)
(delete-selection-mode t)
(setq startup-screen-inhibit-startup-screen t)
(setq inhibit-startup-screen t)
(setq yes-or-no-p 'y-or-n-p)
(setq uniquify-buffer-name-style 'post-forward)
(setq column-number-mode t)
(setq size-indication-mode t)
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil
(icomplete-mode t)
(setq require-final-newline t)
'(recentf-mode)
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;;; Visual configurations
(progn
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(global-linum-mode t)
'(transient-mark-mode t)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq column-number-mode t)
(set-foreground-color "#dbdbdb")
(set-background-color "#000000")
(set-cursor-color "#ff0000")

;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)

;;; Backups
(setq make-backup-files nil)
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

;; Kill buffers
(defun kill-others ()
  "Kill all buffers but this"
  (interactive)
  (mapcar (lambda (buffer)
            (if (not
                  (eq buffer (current-buffer)))
                (kill-buffer buffer)))
            (buffer-list))
  (delete-other-windows))

(defun kill-all-buffers ()
  "Kill all opened buffers but, leaving *scratch* only."
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;;; Key bindings
(global-set-key (kbd "<f12>") 'ido-mode)
(global-set-key (kbd "C-<f4>") 'kill-others)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "<f9>") 'comment-region)
(global-set-key (kbd "<M-f9>") 'uncomment-region)
(global-set-key (kbd "C-z") 'nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(windmove-default-keybindings 'meta) ; Move between windows with ALT + <arrow>

;;; Libraries
(add-to-list 'load-path "~/.emacs.d/vendors/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/") 

;;; Auto-load modes
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^Gemfile$" . ruby-mode))

;;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete/")
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)
(setq ac-dwim 3)
(setq ac-override-local-map nil)
