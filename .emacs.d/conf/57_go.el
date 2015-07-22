;; Go mode
(add-to-list 'exec-path (expand-file-name "~/bin"))


(require 'go-mode-autoloads)
(require 'go-autocomplete)
;;(require 'go-flymake)

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))
