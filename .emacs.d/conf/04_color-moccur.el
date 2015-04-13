;;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;;; color-moccur.elの設定
(require 'color-moccur)

;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

(require 'moccur-edit)

(global-set-key "\M-s" 'moccur-grep-find)

