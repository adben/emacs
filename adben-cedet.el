;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load-file "~/.emacs.d/vendor/cedet/common/cedet.el")
;;(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/lisp/cedet/")
;;(load-file "/Applications/Emacs.app/Contents/Resources/lisp/cedet/cedet.elc");;
;;make all the 'semantic.cache' files go somewhere sane
(require 'semantic)
(setq semanticdb-default-save-directory "~/.emacs-meta/semantic.cache/")
;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
;; (semantic-load-enable-code-helpers 1)
;; (semantic-load-enable-guady-code-helpers 1)
;; (semantic-load-enable-excessive-code-helpers 1)
;; ;; ;;Enable this if you develop in semantic, or develop grammars
;; (semantic-load-enable-semantic-debugging-helpers)
(require 'cedet)
(require 'eieio)
(require 'semantic)
(require 'speedbar)
(require 'srecode)
(require 'ede)

