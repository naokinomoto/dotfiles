;; Go mode
(add-to-list 'exec-path (expand-file-name "~/develop/golang/bin"))
(add-to-list 'load-path (expand-file-name "~/develop/golang/src/github.com/nsf/gocode/emacs"))
(add-to-list 'load-path (expand-file-name "~/develop/golang/src/github.com/dougm/goflymake"))


(require 'go-mode-load)
(require 'go-autocomplete)
(require 'go-flymake)

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))
