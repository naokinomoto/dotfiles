(add-to-list 'exec-path "/usr/local/bin")

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

(add-hook 'after-init-hook #'global-flycheck-mode)

