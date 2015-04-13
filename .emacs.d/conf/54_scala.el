(require 'scala-mode2)

(require 'scala-mode-feature-electric)
(add-hook 'scala-mode-hook
          (lambda ()
            (scala-electric-mode)))

(add-to-list 'load-path "~/.emacs.d/elisp/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

