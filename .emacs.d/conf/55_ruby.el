;(require 'auto-complete-ruby)

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)

(setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.rake$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ru$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gemspec$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("Gemfile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("Rakefile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("Guardfile$" . ruby-mode)) auto-mode-alist))

(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))


(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))


(global-font-lock-mode 1)
(setq default-frame-alist (append '(
  (foreground-color . "gray")  ;
  (background-color . "black") ;
  (cursor-color     . "blue")  ;
)
default-frame-alist))

(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)


;; ruby-block.el
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; flymake-ruby
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode t))
             ))


;; indent
;; http://willnet.in/13
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; haml-mode
;; http://emacswiki.org/emacs/HamlMode
(require 'haml-mode)
(add-hook 'haml-mode-hook
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (define-key haml-mode-map "\C-m" 'newline-and-indent)))
(setq auto-mode-alist (append '(("\\.haml$" . haml-mode)) auto-mode-alist))

;; http://hiroki.jp/2011/03/17/1697/
;; ruby-electric.el
(require 'ruby-electric)
;(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))
