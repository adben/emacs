;; When turning on flyspell-mode, automatically check the entire buffer.
;; Why this isn't the default baffles me.
;;ispell configs for Mac OSX. dutch as default, english configured
;;hunspell
;; (setq ispell-program-name "hunspell")
;; (setq ispell-dictionary-alist
;;   '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US") nil utf-8)))
;; (eval-after-load "ispell"
;;   '(progn (defun ispell-get-coding-system () 'utf-8)))
;; (setq
;;  ispell-dictionary-alist
;;  '((nil				; default 
;;     "[A-Za-z]" "[^A-Za-z]" "[']"
;;     t ("-d" "/opt/local/share/hunspell/en_US") nil iso-8859-1)
;;    ("es_ES"
;;     "[A-Za-z]" "[^A-Za-z]" "[']"
;;     t ("-d" "/opt/local/share/hunspell/es_ES") nil iso-8859-1)
;;    ("nl_NL"
;;     "[a-zA-Z\304\326\334\344\366\337\374]" "[^a-zA-Z\304\326\334\344\366\337\374]" "[']"
;;     t ("-d" "/opt/local/share/hunspell/nl_NL") nil iso-8859-1)))
;; (eval-after-load "ispell"
;;     (progn
;;       (setq ispell-dictionary "nl_NL"
;; 	    ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
;; 	    ispell-silently-savep t)))
;; another

;; (setq-default ispell-program-name "hunspell")
;; (require 'rw-language-and-country-codes) 
;; (require 'rw-ispell) 
;; (require 'rw-hunspell) 
;; (setq rw-hunspell-default-dictionary "nl_NL" 
;;       rw-hunspell-dicpath-list (quote ("/opt/local/share/hunspell/")) 
;;       rw-hunspell-make-dictionary-menu t 
;;       rw-hunspell-use-rw-ispell t 
;;       ispell-program-name "hunspell"
;; ;;(setq ispell-extra-args '("-a" "-i" "utf-8") ;; probably not needed 
;;       ispell-dictionary-alist	 '((nil ; my default is Dutch 
;; 				    "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" 
;;                                     "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ] " "[']" t ("-d" "nl_NL" "-i" 
;;                                                                               "utf-8") nil utf-8) 
;;                                    ("gringo" ; US English 
;;                                     "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" 
;;                                     "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[']" t ("-d" "en_US" "-i" 
;;                                                                              "utf-8") nil utf-8)))
;;other
;; (if (file-exists-p "/opt/local/bin/hunspell")                                
;;     (progn
;;       (setq ispell-program-name "hunspell")
;;       (eval-after-load "ispell"
;;         '(progn (defun ispell-get-coding-system () 'utf-8)))))
;; ;;aspell 
;; (setq ispell-program-name "aspell"
;;       ispell-dictionary-alist
;;       '(("dutch" "[a-zA-Z\304\326\334\344\366\337\374]"
;; 	 "[^a-zA-Z\304\326\334\344\366\337\374]" "[']" t
;; 	 ("-C" "-d" "nederlands" "--dict-dir"
;; 	  "/Library/Application Support/cocoAspell/aspell-nl-0.50-2")
;; 	 "~latin1" iso-8859-1)
;;         (nil
;; 	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
;; 	 ("-B" "-d" "english" "--dict-dir"
;; 	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
;; 	 nil iso-8859-1)
;; 	))
;;The default value is “ispell” which is not what you want. Your aspell has been installed in /usr/local/bin – this directory should be in your ExecPath. If it isn’t, you might have to add it:
;; (autoload 'ispell-word "ispell"
;;          "Check the spelling of word in buffer." t)
;;       (global-set-key "\e$" 'ispell-word)
;;       (autoload 'ispell-region "ispell"
;;          "Check the spelling of region." t)
;;       (autoload 'ispell-buffer "ispell"
;;          "Check the spelling of buffer." t)
;;       (autoload 'ispell-complete-word "ispell"
;;          "Look up current word in dictionary and try to complete it." t)
;;       (autoload 'ispell-change-dictionary "ispell"
;;          "Change ispell dictionary." t)
;;       (autoload 'ispell-message "ispell"
;;          "Check spelling of mail message or news post.")
;;       (autoload 'ispell-minor-mode "ispell"
;;          "Toggle mode to automatically spell check words as they are typed in.")
(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate)
  (flyspell-buffer))
(setq flyspell-issue-welcome-flag nil)

