;; Zen Coding Mode
;; http://www.goodpic.com/mt/archives2/2010/02/emacs_zencoding.html
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;; (add-hook 'html-mode-hook 'zencoding-mode)
;; (add-hook 'text-mode-hook 'zencoding-mode)
;; (define-key zencoding-mode-keymap "\C-z" 'zencoding-expand-line)

(require 'web-mode)

;;; emacs 23以下の互換
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'web-mode-hook)
