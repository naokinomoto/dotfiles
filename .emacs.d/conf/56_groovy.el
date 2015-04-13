;; groovy-mode
;; http://groovy.codehaus.org/Emacs+Plugin
(add-to-list 'load-path "~/.emacs.d/elisp/groovy-mode/")


(require 'groovy-mode)
(require 'groovy-electric)

(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(add-hook 'groovy-mode-hook (lambda ()
                              (require 'groovy-electric)
                              (groovy-electric-mode)))

;; setting indent(c-mode)
;; http://www.gg3721.com/list/49/942858.html
(defun my-groovy-mode-hook ()
  (setq indent-tabs-mode nil
        c-basic-offset 2))

(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)


;; grails-mode
;; http://code.google.com/p/emacs-grails-mode/
(require 'project-mode)
(require 'grails-mode)
(setq grails-mode t)
(setq project-mode t)
(add-to-list 'auto-mode-alist '("\.gsp$" . nxml-mode)) ; Use whatever mode you want for views.
(project-load-all) ; Loads all saved projects. Recommended, but not required.


;; ext. setting for groovy/grails
;; http://blog.udzura.jp/2011/02/10/grails-emacs-development/

;; ac for groovy/grails
(setq ac-modes (append ac-modes '(groovy-mode)))
(setq ac-modes (append ac-modes '(grails-mode)))

;; flymake-groovy
;(require 'flymake-groovy)

