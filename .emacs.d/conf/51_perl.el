;;; Perl

(require 'perlbrew)
(perlbrew-switch "perl-5.16.2")

(add-to-list 'flymake-allowed-file-name-masks
             '("\\(\\.t\\|\\.psgi\\)$" flymake-perl-init))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (perlbrew-get-current-perl-path) (list "-MProject::Libs" "-wc" local-file))))

(add-hook 'cperl-mode-hook (lambda () (flymake-mode t)))


;; cperl-mode
;; (autoload 'cperl-mode "cperl-mode" nil t)
;; (eval-after-load "cperl-mode"
;;   '(progn
;;      ;; indentation
;;      (setq cperl-indent-level 4)
;;      ;; key config
;;      (define-key cperl-mode-map (kbd ";") nil)))
;; (defalias 'perl-mode 'cperl-mode)

;; ;; perl completion
;; (add-hook  'cperl-mode-hook (lambda ()
;;                               (require 'auto-complete)
;;                               (require 'perl-completion)
;;                               (add-to-list 'ac-sources 'ac-source-perl-completion)
;;                               (perl-completion-mode t)))


