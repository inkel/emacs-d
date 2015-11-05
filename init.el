(require 'package)
(require 'server)

;; Do not activate installed packages when Emacs starts.
(setq package-enable-at-startup nil)

;; Additional sources for packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; Load and activate packages
(package-initialize)

;; This article changed my life
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Load custom settings if present
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Start daemon for emacsclient
(unless (server-running-p)
  (server-start))
