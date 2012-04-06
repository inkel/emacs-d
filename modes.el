;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-use-url-at-point nil
      ido-use-filename-at-point nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (quote ("\\` " "\\*\\w+\\*"))
      ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\.git")))

;; Analyze script hash-bang and mark it as executable if possible on
;; first save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Magit
(vendor 'magit)

;; Textmate mode
(vendor 'textmate)
(textmate-mode t)

(provide 'modes)
