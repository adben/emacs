(set 'nxml-path "/usr/share/emacs-snapshot/site-lisp/nxml-mode/")
;;(Load ("/usr/share/emacs-snapshot/site-lisp/nxml-mode/rng-auto.elc"))
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "xhtml") t) "\\'")
		   'nxml-mode))

(unify-8859-on-decoding-mode)

(fset 'xml-mode 'nxml-mode)
