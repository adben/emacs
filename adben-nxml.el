<<<<<<< HEAD
<<<<<<< HEAD
(set 'nxml-path "/usr/share/emacs-snapshot/site-lisp/nxml-mode/")
;;(Load ("/usr/share/emacs-snapshot/site-lisp/nxml-mode/rng-auto.elc"))
=======
=======
>>>>>>> emacs24
(set 'nxml-path "/home/adolfo/soft/emacs/share/emacs/24.0.50/lisp/nxml/")
;;(set 'nxml-path "/usr/share/emacs-snapshot/site-lisp/nxml-mode/")
;;/home/adolfo/soft/emacs/share/emacs/24.0.50/lisp/nxml/
;;(Load ("/usr/share/emacs-snapshot/site-lisp/nxml-mode/rng-auto.elc"))
;;(Load ("/home/adolfo/soft/emacs/share/emacs/24.0.50/lisp/nxml/rng-auto.elc"))
<<<<<<< HEAD
>>>>>>> emacs24
=======
>>>>>>> emacs24
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "xhtml") t) "\\'")
		   'nxml-mode))

(unify-8859-on-decoding-mode)

(fset 'xml-mode 'nxml-mode)
