;; Default font
(condition-case nil
    (set-face-attribute 'default nil :family "Monaco" :height 140))

(when (display-graphic-p)
  ;; Hide toolbar, menu and scrollbars
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (if (file-exists-p "~/.emacs.d/vendor/emacs-color-theme-solarized")
      (progn
        (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
        (load-theme 'solarized-dark))
    (load-theme 'misterioso)))

;; Use unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-strip-common-suffix nil)
(setq uniquify-after-kill-buffer-p t)

;; Highlight parenthesis/brackets
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Highlight matches during query replacement
(setq query-replace-highlight t)

;; Highlight current search match
(setq search-highlight t)

;; Indicate column number on status bar
(setq column-number-mode t)

;; Show buffer size
(setq size-indication-mode t)

;; Misc indicators
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)

;; Frame title format: filename (fullpath)
(setq frame-title-format "%b (%f)")

;; Syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(global-hi-lock-mode t)

;; Indicate line number on the left side
(if (functionp 'global-linum-mode)
    (global-linum-mode t))

;; Blink cursor
(blink-cursor-mode t)

;; Minibuffer
(setq enable-recursive-minibuffers nil ;;  allow mb cmds in the mb
      max-mini-window-height .25       ;;  max 2 lines
      minibuffer-scroll-window nil
      resize-mini-windows nil)

;; Highlight current line
(global-hl-line-mode t)

;; Small fringes
(set-fringe-mode '(1 . 1))

(provide 'visual)
