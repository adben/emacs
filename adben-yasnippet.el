<<<<<<< HEAD
<<<<<<< HEAD
(require 'yasnippet)
;Don't map TAB to yasnippet
;In fact, set it to something we'll never use because
;we'll only ever trigger it indirectly.
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))

(yas/initialize)
=======
=======
>>>>>>> emacs24
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
<<<<<<< HEAD
>>>>>>> emacs24
=======
>>>>>>> emacs24
(yas/load-directory "~/.emacs.d/snippets")



