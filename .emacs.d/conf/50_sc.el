(add-to-list 'load-path "~/.emacs.d/elisp/scel/el")
(require 'sclang)

(custom-set-variables
 '(sclang-indent-level 2)
;;  '(sclang-auto-scroll-post-buffer t)
;;  '(sclang-eval-line-forward nil)
;;  '(sclang-help-path (quote ("/Applications/SuperCollider/SuperCollider.app/Contents/Resource/HelpSource")))
;; ;;  '(sclang-runtime-directory "~/.sclang/")
 )

(push "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/" exec-path)

