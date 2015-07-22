;; key binds

;; key bind for yosemite
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; backspace
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

;;C-^で、カレントなウィンドウのサイズを一つ拡大
(global-set-key "\C-x\C-^" 'enlarge-window})

;;bs-showでバッファ選択する。
(global-set-key "\C-x\C-b" 'bs-show)


;;フルスクリーン(トグル)
;(set-frame-parameter nil 'fullscreen 'fullboth)
(defun my-toggle-frame-size ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (message "Full-screen changed")
    ))
(global-set-key "\C-cm" 'my-toggle-frame-size)

(global-set-key "\C-c\g" 'moccur-grep-find)

;;find-file-other-frame
(global-set-key "\M-n" 'find-file-other-frame)


