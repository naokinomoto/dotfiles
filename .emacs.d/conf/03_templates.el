(require 'autoinsert)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-alist
      (nconc '(
               ("\\.html$" . "tmpl.html")
               ("\\.py$" . "tmpl.py")
               ("\\.pl$" . "tmpl.pl")
               ("\\.rb$" . "tmpl.rb")
               ("\\.php$" . "tmpl.php")
               ) auto-insert-alist))

(add-hook 'find-file-not-found-hooks 'auto-insert)
