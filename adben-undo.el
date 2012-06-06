;; Using the elpa undo tree build
(setq load-path (cons "~/.emacs.d/elpa/undo-tree-20120511/" load-path))
(require 'undo-tree)
(global-undo-tree-mode t)
