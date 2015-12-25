;;; elixir

(require 'elixir-mode)

;; https://github.com/tonini/alchemist.el
(require 'alchemist)

(setq alchemist-iex-program-name "/usr/local/bin/iex")
(setq alchemist-mix-command "/usr/local/bin/mix")
(setq alchemist-execute-command "/usr/local/bin/elixir")
(setq alchemist-compile-command "/usr/local/bin/elixirc")

(setq alchemist-test-status-modeline nil)

(add-hook 'elixir-mode-hook 'ac-alchemist-setup)
(eval-after-load "alchemist"
  #'(progn
      (eval-after-load "elixir-mode"
        #'(progn
            (define-key alchemist-mode-map (kbd "C-x C-e") 'alchemist-iex-send-last-sexp)
            (define-key alchemist-mode-map (kbd "C-M-x") 'alchemist-iex-send-region)
            ))))



