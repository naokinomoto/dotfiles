;; JavaScript

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2)))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)


;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.tss$" . json-mode))
(add-hook 'json-mode-hook 'electric-pair-mode)
(add-hook 'json-mode-hook '(lambda ()
                             (setq js-indent-level 2)))


