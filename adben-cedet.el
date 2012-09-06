;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load-file "~/.emacs.d/vendor/cedet/common/cedet.el")
;;(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/lisp/cedet/")
;;(load-file "/Applications/Emacs.app/Contents/Resources/lisp/cedet/cedet.elc");;
;;make all the 'semantic.cache' files go somewhere sane
;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
;; (semantic-load-enable-code-helpers 1)
;; (semantic-load-enable-guady-code-helpers 1)
;; (semantic-load-enable-excessive-code-helpers 1)
;; ;; ;;Enable this if you develop in semantic, or develop grammars
;; (semantic-load-enable-semantic-debugging-helpers)
(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(semantic-load-enable-excessive-code-helpers)
(global-srecode-minor-mode 1)            ; Enable template insertion menu
;; (require 'cedet)
;; (require 'eieio)
;; (require 'semantic)
;; (require 'speedbar)
;; (require 'srecode)
;; (require 'ede)
;;(setq semanticdb-default-save-directory "~/.emacs-meta/semantic.cache/")



