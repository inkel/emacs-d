;; Raise GC threshold to maximum available
(setq gc-cons-threshold most-positive-fixnum)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Prevent special filename parsing
(let ((file-name-handler-alist nil))

  ;; Custom variables
  (setq custom-file "~/.emacs.d/custom.el")
  (if (file-exists-p custom-file)
      (load custom-file))



  ;; Set GC threshold to 1GB
  (setq gc-cons-threshold (* 1000 1000))
  ) ;; End prevent of special filename parsing
