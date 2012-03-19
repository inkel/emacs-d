(require 'flymake)
;;; (require 'rfringe)

;; Report messages in the minibuffer
;; http://www.emacswiki.org/emacs/FlymakeCursor
(load "~/.emacs.d/inkel/flymake-cursor.el")

;; Colors
(set-face-background 'flymake-errline nil)
(set-face-foreground 'flymake-errline "red4")
(set-face-background 'flymake-warnline nil)
(set-face-foreground 'flymake-warnline "dark slate blue")

;; FlyMake for Emacs Lisp
;; http://www.emacswiki.org/emacs/FlymakeElisp
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval"
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (condition-case data
                  (scan-sexps (point-min) (point-max))
                (scan-error
                 (goto-char(nth 2 data))
                 (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                file (line-number-at-pos)))))))
          )
         )
        local-file)))))

(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)

(add-hook 'emacs-lisp-mode-hook
          ;; workaround for (eq buffer-file-name nil)
          (function (lambda () (if buffer-file-name (flymake-mode)))))

;; Flymake Ruby
;; http://www.emacswiki.org/emacs/FlymakeRuby
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("\\(Guard\\|Rake\\)file$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

;; Flymake JavaScript
;; http://www.emacswiki.org/emacs/FlymakeJavaScript
(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/usr/local/bin/jslint" (list local-file))))

(push '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"
        nil 1 2 3)
      flymake-err-line-patterns)

(add-to-list 'flymake-allowed-file-name-masks '("\\.js\\'" flymake-jslint-init))

(add-hook 'espresso-mode-hook 'flymake-mode)

(provide 'inkel/flymake)
