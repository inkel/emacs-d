;; Stolen from https://github.com/zonuexe/ordinal.el

(defconst inkel/ordinal-english-suffixes '(nil "st" "nd" "rd"))

(defun inkel/ordinal-suffix (n)
  "Return suffix string of `N' in English."
  (let ((last-1-digit (% n 10))
        (last-2-digit (% n 100)))
    (if (memq last-2-digit '(11 12 13))
        "th"
      (or (nth last-1-digit inkel/ordinal-english-suffixes)
          "th"))))

(defconst inkel/theorem-weekly-path "~/dev/citrusbyte/wiki/Theorem-Weekly.md")

(defun inkel/add-theorem-weekly-links ()
  "Automatically add Theorem Weekly links"
  (interactive)
  (let
      ((date (let* ((dayn (string-to-number (format-time-string "%d")))
                     (day (inkel/ordinal-suffix dayn))
                     (fmt (concat "%B " (number-to-string dayn) day ", %Y"))
                     (initial (format-time-string fmt)))
               (read-from-minibuffer "Date: " initial)))
       (presentation (read-from-minibuffer "Presentation: "))
       (recording (read-from-minibuffer "Recording: "))
       (buffer (find-file inkel/theorem-weekly-path)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "# Episodes")
      (end-of-line)
      (insert (concat "\n* " date
                      "\n  * Presentation: " presentation
                      "\n  * Recording: " recording)))))

(defun inkel/git-dance ()
  ""
  (progn
    (magit-stage-file buffer-file-name)))
