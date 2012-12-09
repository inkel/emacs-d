;; My emacs configuration to use it as my Git editor

;; I'm using this configuration in my ~/.gitconfig

;; git config --global core.editor "/usr/local/bin/emacs -q -l ~/.emacs.d/giteditor.el -nw"

;; See https://github.com/lunaryorn/git-modes

(require 'package)
(package-initialize)
(require 'git-commit-mode)
(add-hook 'git-commit-mode-hook (lambda ()
                                  (flyspell-mode t)))
