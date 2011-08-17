;; (when
;;     (load 
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize)
;;   ;;Another repositories for elpa and... (gnus)
;;   (setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
;;                            ("gnu" . "http://elpa.gnu.org/packages/")
;;                            ("technomancy" . "http://repo.technomancy.us/emacs/")
;;                            ("marmalade" . "http://marmalade-repo.org/packages/"))))
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
