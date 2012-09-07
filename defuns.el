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

(defun resize-windows-horizontally-even ()
  "Resizes all the current windows to the most even possible size."
  (interactive)
  (save-excursion
    (let* ((qty (length (window-list)))
           (width (/ (frame-width) qty)))
      (dolist (window (window-list))
        (select-window window)
        (enlarge-window (- width (window-width)) 'horizontal)))))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))

(provide 'defuns)
