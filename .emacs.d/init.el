(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
(setq garbage-collection-messages t)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq debug-on-error t)

(set-language-environment 'japanese)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(use-package cl
  :ensure t
  :defer t)

;; タイトルバーにファイル名を表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
;;選択領域のハイライト
(transient-mark-mode 1)
;; メニューバーを消す
(menu-bar-mode -1)
;; スクロールバーを消す
(scroll-bar-mode -1)
;; ツールバー（アイコン）を消す
(tool-bar-mode 0)
(column-number-mode t)
;; 1行ずつスクロール
(setq scroll-step 1)
;; 対応するカッコを色表示する
(show-paren-mode 1)
;; 行表示
(global-linum-mode t)
(setq linum-format "%4d ")
;; 時間表示
(display-time-mode 1)
;; ヴィジュアルベル無効
(setq visible-bell nil)
;; ビープ音無効
(setq ring-bell-function '(lambda ()))
;;インデントはスペースにする
(setq-default indent-tabs-mode nil)
;;インデント幅
(setq-default c-basic-offset 2)
;;タブ幅
(setq-default default-tab-width 2)
(setq-default tab-width 2)
;; 自動バックアップファイルの未作成
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-inhibited t)
;; 色つける
(global-font-lock-mode t)
(setq-default transient-mark-mode t)
;; wdired
(use-package wdired
  :ensure t
  :defer t
  :bind (:map dired-mode-map ("r" . wdired-change-to-wdired-mode))
  )
;; 改行マーク/全角スペースマーク/タブマーク
(setq whitespace-style
      '(tabs tab-mark spaces space-mark face))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))

(use-package whitespace
  :ensure t
  :defer t
  :config
  (progn
    (global-whitespace-mode 1)
    (set-face-foreground 'whitespace-space "#555555")
    (set-face-background 'whitespace-space nil)
    (set-face-foreground 'whitespace-tab "#555555")
    (set-face-background 'whitespace-tab nil)))




;; 行末の空白をめだたせる M-x delete-trailing-whitespace で削除出来る
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)

;; tramp
(use-package tramp
  :ensure t
  :defer t
  :config
  (progn
    (setq tramp-default-method "ssh")))

;; esup
(use-package esup
  :ensure t
  :defer t)

;; recentf
(recentf-mode t)
;; open-junk-file
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

(find-function-setup-keys)

;; popwin
(use-package popwin
  :ensure t
  :defer t
  :init
  (progn
   (setq display-buffer-function 'popwin:display-buffer)))

;; migemo
;; https://gist.github.com/4176883
(use-package migemo
  :ensure t
  :defer t
  :config
  (prong
     (setq migemo-command "cmigemo")
     (setq migemo-options '("-q" "--emacs"))
     (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
     (setq migemo-user-dictionary nil)
     (setq migemo-regex-dictionary nil)
     (setq migemo-coding-system 'utf-8-unix)
     (load-library "migemo")
     (migemo-init)))

;; cua-mode
(use-package cua-base
  :ensure t
  :defer t
  :init
  (cua-mode t)
  :bind
  (("C-x SPC" . cua-set-rectangle-mark))
  :config
  (progn
    (setq cua-enable-cua-keys nil)))

;; ddskk
(use-package ddskk
  :ensure t
  :defer t
  :bind ("C-x C-j" . skk-mode))

;; neotree
(use-package neotree
  :ensure t
  :defer t
  :bind
  (([f8] . neotree-toggle)))

;;;;;;;;;;;;;;;;;;;;
;; Keybinds

(global-set-key "\C-h" 'backward-delete-char-untabify)

;; backward-delete-word
;; http://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word. With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key "\M-h" 'backward-delete-word)

;;C-x C-gで、指定行に飛ぶ
(global-set-key "\C-x\C-g" 'goto-line)
;;bs-showでバッファ選択する。
(global-set-key "\C-x\C-b" 'bs-show)

(global-set-key "\C-c\g" 'moccur-grep-find)
(global-set-key "\C-j" 'eval-print-last-sexp)

;; window resizer
;; http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
    (current-width (window-width))
    (current-height (window-height))
    (dx (if (= (nth 0 (window-edges)) 0) 1
          -1))
    (dy (if (= (nth 1 (window-edges)) 0) 1
          -1))
    c)
  (catch 'end-flag
    (while t
      (message "size[%dx%dy]"
               (window-width) (window-height))
      (setq c (read-char))
      (cond ((= c ?f)
             (enlarge-window-horizontally dx))
            ((= c ?b)
             (shrink-window-horizontally dx))
            ((= c ?n)
             (enlarge-window dy))
            ((= c ?p)
             (shrink-window dy))
            (t
             (message "Quit")
             (throw 'end-flag t)))))))

(define-key global-map "\C-q" (make-sparse-keymap))
(global-set-key "\C-q\C-q" 'quoted-insert)
(global-set-key "\C-q\C-r" 'window-resizer)
(global-set-key "\C-q\C-f" 'windmove-right)
(global-set-key "\C-q\C-b" 'windmove-left)
(global-set-key "\C-q\C-p" 'windmove-up)
(global-set-key "\C-q\C-n" 'windmove-down)

;; Font

(set-face-attribute 'default nil
             :family "Ricty"
             :height 100)

;; helm
(use-package helm
  :ensure t
  ; :diminish helm-mode "h"
  :bind
  (("M-x"     . helm-M-x)
   ("C-c f"   . helm-mini)
   ("C-c s"   . helm-ag)
   ("C-c C-f" . helm-find-files)
   ("C-c b"   . helm-buffers-list)
   ("C-c i"   . helm-imenu)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   )
  :init
  (progn
    (use-package helm-config)
    (helm-mode 1)))

;; company
(use-package company
  :ensure t
  :defer t
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("C-i" . company-filter-selection))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  (:map emacs-lisp-mode-map
        ("C-M-i" . company-complete)))

;; flycheck
(use-package flycheck
  :ensure t
  :defer t)

;; yasnipet
(use-package yasnippet
  :ensure t
  :defer t)


(use-package w3m
  :ensure t
  :config
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (setq w3m-command "/usr/bin/w3m")
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m")
  (global-set-key "\C-xm" 'browse-url-at-point)
  (setq w3m-use-cookies t)
  (setq w3m-default-display-inline-images t))


(require 'w3m)

;; elisp
(use-package lispxmp
  :ensure t
  :defer t
  :bind (:map emacs-lisp-mode-map ("C-c C-d" . lispxmp)))

(use-package paredit
  :ensure t
  :defer t
  ; :diminish paredit-mode
  :bind (:map paredit-mode-map (("C-j" . eval-print-last-sexp)
                                ("M-p" . paredit-splice-sexp-killing-backward)))
  :hook ((emacs-lisp-mode lisp-interaction-mode lisp-mode ielm-mode scheme-mode extempore-mode) . enable-paredit-mode))

;; Scheme / Gauche
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run ad inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;; Ruby
;; TODO: basic configure only
(use-package ruby-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-indent-level 2)
    (setq ruby-indent-tabs-mode nil)
    (setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.rake$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.ru$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.gemspec$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("Gemfile$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("Rakefile$" . ruby-mode)) auto-mode-alist))
    (setq auto-mode-alist (append '(("Guardfile$" . ruby-mode)) auto-mode-alist))
    (setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
    ))

(use-package inf-ruby
  :ensure t
  :defer t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-block
  :ensure t
  :defer t
  :config
  (progn
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t)))

;; haskell
(use-package haskell-mode
  :ensure t
  :defer t
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :hook (haskell-mode . (lambda ()
                          (turn-on-haskell-indentation)
                          (turn-on-haskell-doc-mode)
                          (font-lock-mode)
                          (imenu-add-menubar-index))))

;; rust
;; flycheck
(use-package flycheck-rust
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :hook ((rust-mode . (lambda ()
                        (racer-mode)
                        (flycheck-rust-setup)
                        ))
         (racer-mode . eldoc-mode)
         (racer-mode . (lambda ()
                         (company-mode)
                         (set (make-variable-buffer-local 'company-idle-delay) 0.3)
                         (set (make-variable-buffer-local 'company-minimum-prefix-length) 1))))
  :init
  (progn
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")))
  :config
  (progn
    (setq-default rust-format-on-save t)))

;; python

;; golang
(use-package go-autocomplete
  :ensure t
  :defer t)

(use-package company-go
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :hook ((before-save . gofmt-before-save)
         (go-mode . flycheck-mode)
         (go-mode . (lambda ()
                      (local-set-key (kbd "M-.") 'godef-jump)
                      (set (make-local-variable 'company-backends) '(company-go))
                      (require 'auto-complete-config)
                      (ac-config-default)
                      (company-mode)
                      (setq indent-tabs-mode nil)
                      (setq c-basic-offset 4)
                      (setq tab-width 4))))
  :init
  (progn
    (add-to-list 'exec-path (expand-file-name "~/bin"))))

;; javascript
(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :hook ((js2-mode . (lambda ()
                       (setq js2-basic-offset 2)))
         ;(js2-mode . ac-js2-mode)
         (js-mode . js2-minor-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.tss$"
  :hook ((json-mode . electric-pair-mode)
         (json-mode . (lambda ()
                        (setq js-indent-level 2)))))

;; typescript
(use-package tide
  :ensure t
  :defer t)

(use-package typescript-mode
  :ensure t
  :defer t
  :hook (typescript-mode . (lambda ()
          (tide-setup)
          (flycheck-mode t)
          (setq flycheck-check-syntax-automatically '(save mode-enabled))
          (eldoc-mode t)
          (company-mode-on))))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(use-package prettier-js
  :ensure t
  :defer t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . (lambda ()
                       (enable-minor-mode
                        '("\\.jsx?\\'" . prettier-js-mode))
                       (enable-minor-mode
                        '("\\.tsx?\\'" . prettier-js-mode))))))


;; web-mode
(defun my-web-mode-hook ()
  (let ((i 2))
    (cond ((string-equal "tsx" (file-name-extension buffer-file-name))
           (setq i 4)
           (setq prettier-js-args '(
                                    "--tab-width" "4"
                                    "--single-quote" "true"
                                    "--no-semi" "false"
                                    ))))
    (setq web-mode-attr-indent-offset nil)
    (setq web-mode-markup-indent-offset i)
    (setq web-mode-css-indent-offset i)
    (setq web-mode-code-indent-offset i)
    (setq web-mode-sql-indent-offset i)
    (setq indent-tabs-mode nil)
    (setq tab-width i)))

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode)
         ("\\.php$"       . web-mode)
         ("\\.jsx$"       . web-mode)
         ("\\.tsx$"       . web-mode))
  :hook (web-mode . my-web-mode-hook)
  :config
  (progn
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

;; supercollider
(use-package sclang
  :load-path "elisp/scel"
  ; :bind (:map sclang-mode-map ("C-j" . sclang-eval-defun))
  :config
  (progn
    (custom-set-variables
     '(sclang-indent-level 2)
     '(sclang-library-configuration-file "~/.local/share/SuperCollider/sclang_conf.yaml"))))

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook (tidal-mode . highlight-numbers-mode))

(use-package tidal
  :load-path "elisp/tidal"
  :bind (:map tidal-mode-map ("C-j" . tidal-run-multiple-lines))
  :config
  (progn
    (setq tidal-interpreter "~/.local/bin/stack")
    (setq tidal-interpreter-arguments
          (list
           "ghci"
           "--ghc-options"
           "-XOverloadedStrings"
           "--ghc-options"
           "-i/home/reprimande/src/github.com/reprimande/liveset/modular/lib"))
    (defun tidal-start-haskell ()
      "Start haskell."
      (interactive)
      (if (comint-check-proc tidal-buffer)
          (error "A tidal process is already running")
        (apply
         'make-comint
         "tidal"
         tidal-interpreter
         nil
         tidal-interpreter-arguments)
        (tidal-see-output))
      (tidal-send-string ":set prompt \"\"")
      (tidal-send-string ":set prompt2 \"\"")
      (tidal-send-string ":module Sound.Tidal.Context")
      (tidal-send-string "import qualified Sound.Tidal.Scales as Scales")
      (tidal-send-string "import qualified Sound.Tidal.Chords as Chords")
      (tidal-send-string ":load /home/reprimande/src/github.com/reprimande/liveset/modular/lib/Liveset/Modular/Stream.hs")
      (tidal-send-string ":load /home/reprimande/src/github.com/reprimande/liveset/modular/lib/Liveset/Modular/Lib.hs")
      (tidal-send-string "import Liveset.Modular.Stream")
      (tidal-send-string "import Liveset.Modular.Lib")
      (tidal-send-string "(cps, nudger, getNow) <- cpsUtils'")
      (tidal-send-string "(d1,t1) <- superDirtSetters getNow")
      (tidal-send-string "(d2,t2) <- superDirtSetters getNow")
      (tidal-send-string "(d3,t3) <- superDirtSetters getNow")
      (tidal-send-string "(d4,t4) <- superDirtSetters getNow")
      (tidal-send-string "(d5,t5) <- superDirtSetters getNow")
      (tidal-send-string "(d6,t6) <- superDirtSetters getNow")
      (tidal-send-string "(d7,t7) <- superDirtSetters getNow")
      (tidal-send-string "(d8,t8) <- superDirtSetters getNow")
      (tidal-send-string "(d9,t9) <- superDirtSetters getNow")
      (tidal-send-string "(d10,t10) <- superDirtSetters getNow")
      (tidal-send-string "(c1,ct1) <- dirtSetters getNow")
      (tidal-send-string "(c2,ct2) <- dirtSetters getNow")
      (tidal-send-string "(c3,ct3) <- dirtSetters getNow")
      (tidal-send-string "(c4,ct4) <- dirtSetters getNow")
      (tidal-send-string "(c5,ct5) <- dirtSetters getNow")
      (tidal-send-string "(c6,ct6) <- dirtSetters getNow")
      (tidal-send-string "(c7,ct7) <- dirtSetters getNow")
      (tidal-send-string "(c8,ct8) <- dirtSetters getNow")
      (tidal-send-string "(c9,ct9) <- dirtSetters getNow")
      (tidal-send-string "(c10,ct10) <- dirtSetters getNow")
      (tidal-send-string "(s0,st0) <- zzzSetters getNow")
      (tidal-send-string "(s1,st1) <- zzzSetters getNow")
      (tidal-send-string "(s2,st2) <- zzzSetters getNow")
      (tidal-send-string "(s3,st3) <- zzzSetters getNow")
      (tidal-send-string "(s4,st4) <- zzzSetters getNow")
      (tidal-send-string "(s5,st5) <- zzzSetters getNow")
      (tidal-send-string "(s6,st6) <- zzzSetters getNow")
      (tidal-send-string "(s7,st7) <- zzzSetters getNow")
      (tidal-send-string "(s8,st8) <- zzzSetters getNow")
      (tidal-send-string "(s9,st9) <- zzzSetters getNow")
      (tidal-send-string "(s10,st10) <- zzzSetters getNow")
      (tidal-send-string "let hush' = mapM_ ($ silence) [s0,s1,s2,s3,s4,s5,s6,s7,s8]")
      (tidal-send-string "let bps x = cps (x/2)")
      (tidal-send-string "let hush = mapM_ ($ silence) [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]")
      (tidal-send-string "let solo = (>>) hush")
      (tidal-send-string ":set prompt \"> \""))))

(add-hook 'font-lock-mode-hook
          '(lambda ()
             (set-face-foreground 'font-lock-constant-face "magenta")))

;; faust
(use-package faust-mode
  :ensure t
  :defer t)

;; GLSL
(use-package glsl-mode
  :ensure t
  :defer t
  :mode (("\\.glsl$" . glsl-mode)
         ("\\.vert$" . glsl-mode)
         ("\\.frag$" . glsl-mode)
         ("\\.geom$" . glsl-mode)))

;; extempore
(use-package extempore-mode
  :ensure t
  :defer t)

;; elixir

;; docker
(use-package dockerfile-mode
  :ensure t
  :defer t)

;; yaml
(use-package yaml-mode
  :ensure t
  :defer t)

;; toml
(use-package toml-mode
  :ensure t
  :defer t)

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (progn
          (setq
           markdown-command "/usr/local/bin/multimarkdown"
           markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                                "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
           markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre[lang] > code').forEach((code) => {
    code.classList.add(code.parentElement.lang);
    hljs.highlightBlock(code);
  });
});
</script>
")))

;; org
;; (use-package org
;;   :ensure t
;;   :defer t
;;   :commands (org-remember-insinuate)
;;   :mode ("\\.org$" . org-mode)
;;   :bind (("\C-cl" . org-store-link)
;;          ("\C-ca" . org-agenda)
;;          ("\C-cr" . org-remember))
;;   :hook (org-mode . turn-on-font-lock)
;;   :config
;;   (progn
;;     ;; 見出しの余分な*を消す
;;     (setq org-hide-leading-stars t)
;;     ;; org-default-notes-file のディレクトリ
;;     (setq org-directory "~/Dropbox/org/")
;;     ;; org-default-notes-file のファイル名
;;     (setq org-default-notes-file "notes.org")
;;     ;; TODO 状態
;;     (setq org-todo-keywords
;;           '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;;     ;; DONE の時刻を記録
;;     (setq org-log-done 'time)
;;     ;; org-remember を使う
;;     (org-remember-insinuate)
;;     ;; org-remember のテンプレート
;;     (setq org-remember-templates
;;           '(("Note" ?n "* %?\n  %i\n  %a" nil "Tasks")
;;             ("Todo" ?t "* TODO %?\n  %i\n  %a" nil "Tasks")))))

;;;;;;;;
;; TODO
;; tags
;; flymake
;; yasnippet


;; for mac
(when (eq system-type 'darwin)
  ;; key bind
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  ;; copy & paste sharing OS
  (defun copy-from-osx ()
    (let ((tramp-mode nil)
          (default-directory "~"))
      (shell-command-to-string "pbpaste")))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; theme
(when (eq window-system 'mac)
  (use-package atom-one-dark-theme
    :ensure t)
  (load-theme 'atom-one-dark t))

;; emacs server
(use-package server
  :ensure t
  :defer t
  :init
  (server-mode 1)
  :config
  (progn
    (unless (server-running-p)
    (server-start))))
