(setq load-path (cons "~/.emacs.d/elpa/auto-complete-20120917.1341/" load-path))
;; Load my auto-complete, over-riding the one bundled with emacs
(require 'auto-complete)
(global-auto-complete-mode t)
