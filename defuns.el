(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el")))
    (cond
     ((file-directory-p normal)
      (add-to-list 'load-path normal)
      (require library))
     ((file-directory-p suffix)
      (add-to-list 'load-path suffix)
      (require library))
     ((file-exists-p suffix)
      (require library)))))

(defun toggle-comment-line ()
  "Toggle comment on current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark (point-at-eol) nil t)
    (comment-dwim t)
    (pop-mark)))

(provide 'defuns)
