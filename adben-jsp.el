 ;; ;; set up an mmm group for fancy html editing
 ;;    (mmm-add-group
 ;;     'fancy-html
 ;;     '(
 ;;       (html-php-tagged
 ;;        :submode php-mode
 ;;        :face mmm-code-submode-face
 ;;        :front "<[?]php"
 ;;        :back "[?]>")
 ;;       (html-css-attribute
 ;;        :submode css-mode
 ;;        :face mmm-declaration-submode-face
 ;;        :front "styleNO=\""
 ;;        :back "\"")
 ;;       (jsp-code
 ;;        :submode java
 ;;        :match-face (("<%!" . mmm-declaration-submode-face)
 ;;    		 ("<%=" . mmm-output-submode-face)
 ;;    		 ("<%"  . mmm-code-submode-face))
 ;;        :front "<%[!=]?"
 ;;        :back "%>"
 ;;        :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
 ;;    	     (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
 ;;    	     (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
 ;;        )
 ;;       (jsp-directive
 ;;        :submode text-mode
 ;;        :face mmm-special-submode-face
 ;;        :front "<%@"
 ;;        :back "%>"
 ;;        :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
 ;;        )
 ;;       ))
