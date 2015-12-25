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


(define-key global-map "\C-c" (make-sparse-keymap))
(global-set-key "\C-c\C-q" 'quoted-insert)
(global-set-key "\C-c\C-r" 'window-resizer)
(global-set-key "\C-c\C-f" 'windmove-right)
(global-set-key "\C-c\C-b" 'windmove-left)
(global-set-key "\C-c\C-p" 'windmove-up)
(global-set-key "\C-c\C-n" 'windmove-down)


