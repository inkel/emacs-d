;; Twitter
;;
;; http://www.emacswiki.org/emacs/TwitteringMode
(defun inkel/open-twitter ()
  "Open up my Twitter timeline"
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/vendors/twittering-mode/")
  (setq twittering-use-master-password t
        twittering-icon-mode t          ; Show icons
        twittering-timer-interval 300   ; Update your timeline each 300 seconds (5 minutes)
        twittering-url-show-status nil) ; Keeps the echo area from showing all the http processes
  (require 'twittering-mode)
  (twit))

(add-hook 'twittering-mode-hook
          (lambda ()
            (linum-mode nil)))

;; Check spelling
(add-hook 'twittering-edit-mode-hook
          (lambda ()
            (ispell-minor-mode)
            (flyspell-mode)))

;; Jabber
(defun inkel/open-jabber ()
  "Start Jabber"
  (interactive)
  (require 'jabber-autoloads)
  (jabber-connect)
  (switch-to-buffer "-*-jabber-roster-*-"))

;; ERC
;; http://www.emacswiki.org/emacs/ERC
(defun inkel/erc-autojoin ()
  (require 'erc-join)
  (erc-autojoin-mode 1)
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#rubysur" "#rubyconfar"))))

(defun inkel/erc-freenode ()
  "IRC on freenode.net"
  (interactive)
  (inkel/erc-autojoin)
  (erc :server "irc.freenode.net"
       :port 6667
       :nick "inkel"))

(provide 'inkel/social)
