;;; newsticker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (newsticker-show-news newsticker-start-ticker newsticker-start
;;;;;;  newsticker-ticker-running-p newsticker-running-p) "newsticker"
;;;;;;  "newsticker.el" (19511 9525))
;;; Generated autoloads from newsticker.el

(autoload 'newsticker-running-p "newsticker" "\
Check whether newsticker is running.
Return t if newsticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not empty.

\(fn)" nil nil)

(autoload 'newsticker-ticker-running-p "newsticker" "\
Check whether newsticker's actual ticker is running.
Return t if ticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not
empty.

\(fn)" nil nil)

(autoload 'newsticker-start "newsticker" "\
Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already.

\(fn &optional DO-NOT-COMPLAIN-IF-RUNNING)" t nil)

(autoload 'newsticker-start-ticker "newsticker" "\
Start newsticker's ticker (but not the news retrieval).
Start display timer for the actual ticker if wanted and not
running already.

\(fn)" t nil)

(autoload 'newsticker-show-news "newsticker" "\
Switch to newsticker buffer.  You may want to bind this to a key.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("newsticker-pkg.el") (19511 9525 624329))

;;;***

(provide 'newsticker-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; newsticker-autoloads.el ends here
