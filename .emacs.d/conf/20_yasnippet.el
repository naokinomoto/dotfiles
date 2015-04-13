;; http://fukuyama.co/yasnippet

;; yasnippetへのロードパス
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elisp/yasnippet"))

(require 'yasnippet)

;; snippetsディレクトリの設定
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; 自作snippets
        "~/.emacs.d/elisp/yasnippet/snippets" ;; デフォルトsnippets
        ))

(yas-global-mode 1)

;; 単語展開キーバインド
(custom-set-variables '(yas/trigger-key "TAB"))

;; 既存snippets挿入
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規snippetsを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存snippetsを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; anything interface
;; https://github.com/sugyan/dotfiles/blob/master/.emacs.d/conf/04-yasnippet.el

(eval-after-load "anything-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (anything-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                        "*anything yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

