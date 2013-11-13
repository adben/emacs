(require 'cider)
;;Enable eldoc in Clojure buffers:
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;;You can hide the *nrepl-connection* and *nrepl-server* buffers from
;;appearing in some buffer switching commands like switch-to-buffer(C-x b) like this:
(setq nrepl-hide-special-buffers t)
;;Stop the error buffer from popping up while working in buffers other than the REPL:
(setq cider-popup-stacktraces nil)
;;To auto-select the error buffer when it's displayed:
(setq cider-auto-select-error-buffer t)
;;The REPL buffer name takes the format *cider project-name*. Change the separator
;;from space to something else by overriding nrepl-buffer-name-separator.
(setq nrepl-buffer-name-separator "-")
;;The use of paredit when editing Clojure (or any other Lisp) code is highly recommended.
;;You're probably using it already in your clojure-mode buffers.
;; You might also want to enable paredit in the REPL buffer as well:
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;;RainbowDelimiters is a minor mode which highlights parentheses, brackets, and braces
;;according to their depth. Each successive level is highlighted in a different color. 
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
