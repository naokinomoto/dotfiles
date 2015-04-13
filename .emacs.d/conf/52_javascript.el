(setq js3-mirror-mode t)
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-hook 'js3-mode-hook
          (lambda ()
            (setq js3-indent-level 2)
            (setq js3-mode-dev-mode-p t)
            (setq js3-auto-indent-p t)
            (setq js3-enter-indents-newline t)
            (setq js3-indent-on-enter-key t)
            (setq js3-consistent-level-indent-inner-bracket t)
            (when (require 'auto-complete nil t)
              (make-variable-buffer-local 'ac-sources)
              (add-to-list 'ac-sources 'ac-source-yasnippet)
              (auto-complete-mode t))))


;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.tss$" . json-mode))
(add-hook 'json-mode-hook 'electric-pair-mode)
(add-hook 'json-mode-hook '(lambda ()
                 (setq js-indent-level 2)
                 ))
