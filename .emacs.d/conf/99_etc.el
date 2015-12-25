;; ;; gtags
;; (require 'gtags)
;; (add-hook 'c-mode-common-hook 'gtags-mode)

;; recentf
(recentf-mode t)

;; open-junk-file
;; http://d.hatena.ne.jp/rubikitch/20080923/1222104034
(defun open-junk-file ()
  (interactive)
  (let* ((file (expand-file-name
                (format-time-string
                 "%Y/%m/%Y-%m-%d-%H%M%S." (current-time))
                "~/memo/junk/"))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file-other-window (read-string "Junk Code: " file))))
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;; lispxmp
;; http://d.hatena.ne.jp/rubikitch/20090313/lispxmp
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; paredit
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(show-paren-mode t)
(global-set-key "\C-m" 'newline-and-indent)
(find-function-setup-keys)

;; popwin
;; https://github.com/m2ym/popwin-el
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;(setq display-buffer-function 'popwin:special-display-config)

;; for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; ELPA
;; http://d.hatena.ne.jp/naoya/20130107/1357553140

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; theme
(load-theme 'atom-dark t)

(color-theme-initialize)
(color-theme-ld-dark)
(color-theme-dark-laptop)

;; ;; elscreen
;; ;; https://github.com/shosti/elscreen
;(setq elscreen-prefix-key "\C-t")
;(elscreen-start)


;; migemo
;; https://gist.github.com/4176883
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; 鬼軍曹.el
;; https://github.com/k1LoW/emacs-drill-instructor/wiki
;;(require 'drill-instructor)
;;(setq drill-instructor-global t)


;; ddskk

;; load-path 追加
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; skkの設定
(require 'skk-autoloads)
(global-set-key (kbd "C-x C-j") 'skk-mode)

;; direx
;; http://cx4a.blogspot.jp/2011/12/popwineldirexel.html
(require 'direx)
(push '(direx:direx-mode :position left :width 60 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-c C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-c C-p") 'direx-project:jump-to-project-root-other-window)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


