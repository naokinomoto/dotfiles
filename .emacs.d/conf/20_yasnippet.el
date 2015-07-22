(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet-snippets"
        )
      )
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


