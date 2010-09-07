;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/elpa/yasnippet-bundle-0.6.1/yasnippet-bundle.el")
;;(require 'yasnippet)
;Don't map TAB to yasnippet
;In fact, set it to something we'll never use because
;we'll only ever trigger it indirectly.
;;(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
;;(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")



