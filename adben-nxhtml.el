(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "xhtml" "tpl") t) "\\'")
		   'nxml-mode))

