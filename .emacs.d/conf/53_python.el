(add-to-list 'load-path "~/.emacs.d/elisp/pymacs/")

(require 'python)
(require 'pymacs)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;http://d.hatena.ne.jp/cou929_la/20110525/1306321857
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(add-hook 'python-mode-hook '(lambda () 
                               (define-key python-mode-map "\C-m" 'newline-and-indent)))

(require 'ac-python)
(add-to-list 'ac-modes 'python-mode)

;; Initialize Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;        (setq value (cons (format "%s%s" prefix element) value))))))

;; (defvar ac-source-rope '((candidates . (lambda ()
;;                                          (prefix-list-elements (rope-completions) ac-target)))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;            (add-to-list 'ac-sources 'ac-source-rope)
;;             (add-to-list 'ac-sources 'ac-source-yasnippet)
;;             ))
