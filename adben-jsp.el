;; ;;multi-mode, used in order to provide the content completion in the jsp mode
;; (load-library "multi-mode")
;; (require 'multi-mode)
;; (autoload 'multi-mode
;;   "multi-mode"
;;   "Allowing multiple major modes in a buffer."
;;   t)

;; (defun jsp-mode () (interactive)
;;   (multi-mode 1
;;               'html-mode
;;               ;;your choice of modes for java and html
;;               ;;'("<%" java-mode)
;;               '("<%" jde-mode)
;;               '("%>" html-mode)))

;; (setq auto-mode-alist
;;       (cons '("\\.jsp$" . jsp-mode)
;;             auto-mode-alist))

;; set up an mmm group for fancy html editing
(require 'mmm-mode)
;; set up an mmm group for fancy html editing
(mmm-add-group
 'fancy-html
 '(
   (html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]php"
    :back "[?]>")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "styleNO=\""
    :back "\"")
   (jsp-code
    :submode java
    :match-face (("<%!" . mmm-declaration-submode-face)
    		 ("<%=" . mmm-output-submode-face)
    		 ("<%"  . mmm-code-submode-face))
    :front "<%[!=]?"
    :back "%>"
    :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
    	     (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
    	     (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (jsp-directive
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
    )
   ))