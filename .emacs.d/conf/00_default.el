;; 言語設定
(set-language-environment 'japanese)

;; Ricty
(set-face-attribute 'default nil
             :family "Ricty" ;; font
             :height 140)  ;; font size
;; 日本語をヒラギノ角ゴProNにする
(if (eq window-system 'ns)
    (progn
      (set-fontset-font "fontset-default"
                        'japanese-jisx0208
                        '("Hiragino Kaku Gothic ProN"))
      (set-fontset-font "fontset-default"
                        'japanese-jisx0212
                        '("Hiragino Kaku Gothic ProN"))
      (set-fontset-font "fontset-default"
                        'japanese-jisx0213-1
                        '("Hiragino Kaku Gothic ProN"))
      (set-fontset-font "fontset-default"
                        'japanese-jisx0213-2
                        '("Hiragino Kaku Gothic ProN"))
      (set-fontset-font "fontset-default"
                        'katakana-jisx0201
                        '("Hiragino Kaku Gothic ProN"))
      ))


(setq backup-inhibited t)

(require 'cl)

;; どのファイルでエラーが出たかわかるように
;; http://d.hatena.ne.jp/kitokitoki/20101205/p1
(defun init-loader-re-load (re dir &optional sort)
  (let ((load-path (cons dir load-path)))
    (dolist (el (init-loader--re-load-files re dir sort))
      (condition-case e
          (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
            (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
        (error
         ;; (init-loader-error-log (error-message-string e)) ；削除
         (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e))) ;追加
         )))))

;; タイトルバーにファイル名を表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 背景色
;; http://www.kutsulog.com/?p=288
;; (if window-system
;;     (progn
;;       ;; 文字の色を設定します。
;;       (add-to-list 'default-frame-alist '(foreground-color . "LightGray"))
;;       ;; 背景色を設定します。
;;       (add-to-list 'default-frame-alist '(background-color . "black"))
;;       ;; カーソルの色を設定します。
;;       (add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
;;       ;; マウスポインタの色を設定します。
;;       (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
;;       ;; モードラインの文字の色を設定します。
;;       (set-face-foreground 'mode-line "white")
;;       ;; モードラインの背景色を設定します。
;;       (set-face-background 'mode-line "purple")
;;       ;; 選択中のリージョンの色を設定します。
;;       ;(set-face-background 'region "LightSteelBlue1")
;;       ;; モードライン（アクティブでないバッファ）の文字色を設定します。
;;       (set-face-foreground 'mode-line-inactive "gray30")
;;       ;; モードライン（アクティブでないバッファ）の背景色を設定します。
;;       (set-face-background 'mode-line-inactive "gray85")
;;       ))

;;選択領域のハイライト
;;(transient-mark-mode 1)


(setq debug-on-error t)


;;矩形領域の選択
;(autoload 'sense-region-on "sense-region"
;  "System to toggle region and rectangle." t nil)
;(sense-region-on)

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバー（アイコン）を消す
(tool-bar-mode 0)

;;1行ずつスクロール
(setq scroll-conservatively 1)

;; 対応するカッコを色表示する
(show-paren-mode 1)

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
(setq-default default-tab-width 4)
(setq-default tab-width 4)

(put 'upcase-region 'disabled nil)

;; 自動バックアップファイルの未作成
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 色つける
;; (global-font-lock-mode t)
;; (setq-default transient-mark-mode t)
;; (require 'font-lock)

;; utf-8優先
(prefer-coding-system 'utf-8)

;; 一行ずつスクロール
(setq scroll-step 1)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)

;; uniquify バッファのディレクトリ表示
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; color-moccur
(require 'color-moccur)
(setq moccur-split-word t)

;; moccur-edit
;; (require 'moccur-edit)
;; (setq moccur-split-word t)


;; gist
(require 'gist)

;; 改行マーク、全角スペースマーク、タブマーク
;; http://weboo-returns.com/blog/emacs-shows-double-space-and-tab/
(setq whitespace-style
      '(tabs tab-mark spaces space-mark face))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "#555555")
(set-face-background 'whitespace-space nil)
(set-face-foreground 'whitespace-tab "#555555")
(set-face-background 'whitespace-tab nil)

;; 行末の空白をめだたせる M-x delete-trailing-whitespaceで削除出来る
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))

;; 問い合わせ簡略化 yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;(require 'w3m)

;; Copy & Paste Sharing OS X
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
(setq interprogram-paste-function 'copy-from-osx)


